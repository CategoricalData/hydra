-- TODO(#350): REMOVE THIS SHIM. It is a PARTIAL, demo-only port of the
-- JSON Schema coder. Tracking issue:
--   https://github.com/CategoricalData/hydra/issues/350
--
-- Background. The Sources DSL spec for the JSON Schema coder lives at
--   packages/hydra-ext/src/main/haskell/Hydra/Sources/Json/Schema/Coder.hs
-- but its host-language implementation has not landed: the generated
--   dist/haskell/hydra-ext/src/main/haskell/Hydra/Json/Schema/Coder.hs
-- ships only recursive stubs (e.g. moduleToJsonSchema opts mod defs cx g
-- = moduleToJsonSchema opts mod defs cx g), so writeJsonSchema in
-- heads/haskell/.../ExtGeneration.hs diverges if invoked. The last
-- commit on the repo with a complete, working hand-written coder is
--   58e198a71  "Update the Scala and JSON Schema coders to use the new adapter framework (#236)"
-- specifically the file
--   hydra-ext/src/main/haskell/Hydra/Ext/Staging/Json/Schema/Coder.hs.
-- Use that commit as the reference when finishing #350; do not treat
-- the present file as a complete starting point.
--
-- Scope of this shim. Just enough to produce demo-grade JSON Schema
-- artifacts for hydra.pg.model.{Graph,GraphSchema} in demos/pg-formats.
-- Known omissions / divergences from the 58e198a71 implementation:
--
--   * @key annotation excludeAnnotatedFields filter is NOT applied
--     (the old coder dropped fields tagged hydra.core.exclude before
--     encoding); we emit all fields.
--   * Short-name substitution (Rewriting.toShortNames) is NOT applied;
--     $defs keys use the local name only via qualifyName, and there
--     is no collision detection across namespaces.
--   * Per-type document layout differs: the old coder produced one
--     file per type via Names.namespaceToFilePath; this shim produces
--     a flat M.Map String String keyed by local name, leaving on-disk
--     layout to the caller (the demo driver).
--   * Union encoding does NOT split off "simple" (unit-typed) variants
--     into a oneOf with a string-enum branch; every union is encoded
--     as a one-of-fields object regardless of variant arity.
--   * forall-quantified types: the old coder did not handle TypeForall
--     because the 0.13-era kernel did not expose it. Here we strip
--     foralls; bound type variables become $ref restrictions whose
--     target schema is absent (an "open" reference).
--   * Type applications: stripped to the head's named type; the schema
--     does not parameterize over the argument.
--   * No @key key annotations of any kind are read (description is
--     extracted directly from the annotated-term map below).
--
-- Net effect: the emitted JSON Schema for a polymorphic type like
-- Graph<v> is a "Graph<any v>" view -- adequate for validating
-- Hydra-JSON where v is encoded as hydra.core.Literal or similar, but
-- not a faithful reproduction of the System F signature, and not a
-- full re-implementation of the older coder's behaviour. When #350
-- lands, replace callers of moduleToJsonSchemaDocs with
-- Hydra.ExtGeneration.writeJsonSchema and delete this file.

module Hydra.Demos.PgFormats.JsonSchemaShim (
  moduleToJsonSchemaDocs,
) where

import qualified Hydra.Core as Core
import qualified Hydra.Json.Schema as JS
import qualified Hydra.Json.Schema.Serde as JsonSchemaSerde
import qualified Hydra.Strip as Strip
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Errors as Errors

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


-- | Convert a Hydra module to a map from type-local-name to JSON Schema document string.
--   Each entry is a self-contained JSON Schema 2020-12 document with $defs for every
--   type referenced from the root.
moduleToJsonSchemaDocs :: Packaging.Module -> Either Errors.Error (M.Map String String)
moduleToJsonSchemaDocs mod = do
    let typeDefs = [td | Packaging.DefinitionType td <- Packaging.moduleDefinitions mod]
    docs <- mapM (toDocPair typeDefs) typeDefs
    return $ M.fromList docs
  where
    toDocPair allDefs td = do
      let rootName = Packaging.typeDefinitionName td
      let rootType = stripTypeScheme (Packaging.typeDefinitionTypeScheme td)
      doc <- buildDocument allDefs rootName rootType
      return (Core.unName rootName, JsonSchemaSerde.jsonSchemaDocumentToString doc)

stripTypeScheme :: Core.TypeScheme -> Core.Type
stripTypeScheme (Core.TypeScheme _ body _) = body

-- | Build a self-contained JSON Schema document with $defs for transitive deps.
buildDocument :: [Packaging.TypeDefinition] -> Core.Name -> Core.Type
              -> Either Errors.Error JS.Document
buildDocument allDefs rootName rootType = do
    let typeMap = M.fromList [(Packaging.typeDefinitionName td,
                               stripTypeScheme (Packaging.typeDefinitionTypeScheme td))
                             | td <- allDefs]
    let depNames = collectDeps typeMap rootType
    let allNames = rootName : L.filter (/= rootName) depNames
    schemas <- mapM (mkPair typeMap) allNames
    return $ JS.Document
      Nothing
      (Just (M.fromList schemas))
      (JS.Schema [referenceRestriction rootName])
  where
    mkPair tm n = case M.lookup n tm of
      Just t  -> do
        restrictions <- encodeNamedType n t
        return (JS.Keyword (encodeName (Packaging.qualifiedNameLocal (qualify n))),
                JS.Schema restrictions)
      Nothing ->
        -- Unbound name (free type variable, typically from a `forall v. ...`
        -- whose body we strip): emit a permissive empty-object schema with
        -- only a title, so $ref lookups resolve to "any value".
        return (JS.Keyword (encodeName (Packaging.qualifiedNameLocal (qualify n))),
                JS.Schema [JS.RestrictionTitle (Core.unName n)])

-- | Walk a type, collecting all transitive named-type references (excluding the root).
collectDeps :: M.Map Core.Name Core.Type -> Core.Type -> [Core.Name]
collectDeps typeMap rootType = go [] [] (typeRefs rootType)
  where
    go acc _    []     = L.reverse acc
    go acc seen (n:ns)
      | n `elem` seen = go acc seen ns
      | otherwise =
          let seen' = n : seen
              nextRefs = case M.lookup n typeMap of
                Just t  -> typeRefs t
                Nothing -> []
          in go (n : acc) seen' (ns ++ nextRefs)

-- | Direct named-type references occurring in a type expression.
typeRefs :: Core.Type -> [Core.Name]
typeRefs t0 = case Strip.deannotateType t0 of
  Core.TypeVariable n          -> [n]
  Core.TypeApplication (Core.ApplicationType f a)
                               -> typeRefs f ++ typeRefs a
  Core.TypeForall (Core.ForallType _ b)
                               -> typeRefs b
  Core.TypeList l              -> typeRefs l
  Core.TypeSet s               -> typeRefs s
  Core.TypeMap (Core.MapType k v)
                               -> typeRefs k ++ typeRefs v
  Core.TypeMaybe m             -> typeRefs m
  Core.TypePair (Core.PairType a b)
                               -> typeRefs a ++ typeRefs b
  Core.TypeEither (Core.EitherType a b)
                               -> typeRefs a ++ typeRefs b
  Core.TypeFunction (Core.FunctionType a b)
                               -> typeRefs a ++ typeRefs b
  Core.TypeRecord fields       -> concatMap (typeRefs . Core.fieldTypeType) fields
  Core.TypeUnion fields        -> concatMap (typeRefs . Core.fieldTypeType) fields
  Core.TypeWrap inner          -> typeRefs inner  -- TypeWrap Type (no name)
  _                            -> []

qualify :: Core.Name -> Packaging.QualifiedName
qualify name =
    let s = Core.unName name
    in case L.elemIndices '.' s of
        [] -> Packaging.QualifiedName Nothing s
        ixs -> let lastDot = L.last ixs
                   nsPart  = L.take lastDot s
                   locPart = L.drop (lastDot + 1) s
               in Packaging.QualifiedName (Just (Packaging.Namespace nsPart)) locPart

encodeName :: String -> String
encodeName = Formatting.nonAlnumToUnderscores

referenceRestriction :: Core.Name -> JS.Restriction
referenceRestriction name = JS.RestrictionReference $ JS.SchemaReference $
    "#/$defs/" ++ encodeName (Packaging.qualifiedNameLocal (qualify name))

-- | Encode a named type as a sequence of JSON Schema restrictions, prefixed with a title.
encodeNamedType :: Core.Name -> Core.Type -> Either Errors.Error [JS.Restriction]
encodeNamedType name typ = do
    res <- encodeType False (Strip.deannotateType typ)
    return $ [JS.RestrictionTitle (Core.unName name)] ++ res

-- | Encode a Hydra type as a sequence of JSON Schema restrictions.
--   The Bool flag marks the type as optional (i.e. inside a TypeMaybe), in which
--   case scalar types are widened to allow null.
encodeType :: Bool -> Core.Type -> Either Errors.Error [JS.Restriction]
encodeType optional typ = case typ of
    Core.TypeAnnotated (Core.AnnotatedType body anns) -> do
      res <- encodeType optional (Strip.deannotateType body)
      let descKey = Core.Name "description"
      let mdesc = case M.lookup descKey anns of
            Just (Core.TermLiteral (Core.LiteralString s)) -> Just s
            _ -> Nothing
      let desc = Y.maybe [] (\d -> [JS.RestrictionDescription d]) mdesc
      return $ desc ++ res
    Core.TypeForall (Core.ForallType _ body) -> encodeType optional body
    Core.TypeApplication (Core.ApplicationType f _) -> encodeType optional f
    Core.TypeList lt -> do
      elSchema <- JS.Schema <$> encodeType False lt
      return $ jsType optional JS.TypeNameArray ++
        [JS.RestrictionArray $ JS.ArrayRestrictionItems $ JS.ItemsSameItems elSchema]
    Core.TypeLiteral lt -> case lt of
      Core.LiteralTypeBinary    -> return $ jsType optional JS.TypeNameString
      Core.LiteralTypeBoolean   -> return $ jsType optional JS.TypeNameBoolean
      Core.LiteralTypeFloat _   -> return $ jsType optional JS.TypeNameNumber
      Core.LiteralTypeInteger _ -> return $ jsType optional JS.TypeNameInteger
      Core.LiteralTypeString    -> return $ jsType optional JS.TypeNameString
    Core.TypeMap (Core.MapType _ v) -> do
      vschema <- JS.Schema <$> encodeType False v
      return $ jsType optional JS.TypeNameObject ++
        [JS.RestrictionObject $ JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsSchema vschema]
    Core.TypeMaybe inner -> encodeType True inner
    Core.TypeRecord fields -> encodeRecordOrUnion optional False fields
    Core.TypeUnion fields  -> encodeRecordOrUnion optional True fields
    Core.TypeSet st -> do
      elSchema <- JS.Schema <$> encodeType False st
      return $ jsType optional JS.TypeNameArray ++
        [JS.RestrictionArray $ JS.ArrayRestrictionItems $ JS.ItemsSameItems elSchema]
    Core.TypeUnit -> return $ jsType optional JS.TypeNameObject
    Core.TypeVariable n -> return [referenceRestriction n]
    Core.TypeWrap inner -> encodeType optional inner
    Core.TypePair (Core.PairType a b) -> do
      ar <- JS.Schema <$> encodeType False a
      br <- JS.Schema <$> encodeType False b
      return $ jsType optional JS.TypeNameObject ++
        [JS.RestrictionObject $ JS.ObjectRestrictionProperties $
            M.fromList [(JS.Keyword "first", ar), (JS.Keyword "second", br)],
         JS.RestrictionObject $ JS.ObjectRestrictionRequired
            [JS.Keyword "first", JS.Keyword "second"],
         JS.RestrictionObject $ JS.ObjectRestrictionAdditionalProperties $
            JS.AdditionalItemsAny False]
    other -> Left $ Errors.ErrorOther $ Errors.OtherError $
      "JsonSchemaShim: unsupported type variant: " ++ show other

encodeRecordOrUnion :: Bool -> Bool -> [Core.FieldType]
                    -> Either Errors.Error [JS.Restriction]
encodeRecordOrUnion optional isUnion fields = do
    props <- mapM encodeField fields
    -- For unions, no field is "required" -- exactly one variant must be present,
    -- which is expressed by min/maxProperties=1, not by JSON Schema's "required".
    let required = if isUnion then [] else Y.catMaybes (map ifReq fields)
    let baseObj  = jsType optional JS.TypeNameObject
                 ++ [JS.RestrictionObject $ JS.ObjectRestrictionProperties $ M.fromList props]
    let reqObj   = if L.null required
                     then []
                     else [JS.RestrictionObject $ JS.ObjectRestrictionRequired required]
    let cardObj  = JS.RestrictionObject <$>
          [JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsAny False]
          ++ if isUnion
               then [JS.ObjectRestrictionMinProperties 1, JS.ObjectRestrictionMaxProperties 1]
               else []
    return $ baseObj ++ reqObj ++ cardObj
  where
    ifReq f = if isRequiredField f
                 then Just (JS.Keyword (Core.unName (Core.fieldTypeName f)))
                 else Nothing

encodeField :: Core.FieldType -> Either Errors.Error (JS.Keyword, JS.Schema)
encodeField (Core.FieldType n t) = do
    res <- encodeType False t
    return (JS.Keyword (Core.unName n), JS.Schema res)

isRequiredField :: Core.FieldType -> Bool
isRequiredField (Core.FieldType _ t) = case Strip.deannotateType t of
  Core.TypeMaybe _ -> False
  _                -> True

jsType :: Bool -> JS.TypeName -> [JS.Restriction]
jsType optional tn =
  [JS.RestrictionType $ if optional
                          then JS.TypeMultiple [tn, JS.TypeNameNull]
                          else JS.TypeSingle tn]
