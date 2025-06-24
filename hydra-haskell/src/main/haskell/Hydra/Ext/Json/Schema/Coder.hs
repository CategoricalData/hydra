module Hydra.Ext.Json.Schema.Coder (
  JsonSchemaOptions(..),
  moduleToJsonSchemaFiles,
) where

import Hydra.Kernel
import Hydra.TermAdapters
import Hydra.Staging.Adapters
import Hydra.Ext.Json.Schema.Language
import Hydra.Ext.Json.Schema.Serde
import qualified Hydra.Ext.Org.Json.Schema as JS
import qualified Hydra.Json as J
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.Annotations

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data JsonSchemaOptions = JsonSchemaOptions {
  jsonSchemaOptionsShortNames :: Bool
}

constructModule
  :: JsonSchemaOptions
  -> Module
  -> M.Map Type (Coder Graph Graph Term ())
  -> [(Element, TypedTerm)]
  -> Flow Graph (M.Map FilePath JS.Document)
constructModule opts mod coders pairs = M.fromList <$> CM.mapM toDocument pairs
  where
    toDocument (el, tt@(TypedTerm term typ)) = if isNativeType el
      then typeTermToDocument (elementName el)
      else fail $ "mapping of non-type elements to JSON Schema is not yet supported: " ++ unName (elementName el)

    typeTermToDocument rootName = do
      nt <- typeDependencies False excludeAnnotatedFields rootName
      let names = M.keys nt
      let nameSubst = toShortNames names
      let types = substituteTypeVariables nameSubst <$> M.elems nt
      schemas <- M.fromList <$> (CM.zipWithM typeToKeywordDocumentPair (substName nameSubst <$> names) types)

      return (nameToPath rootName, JS.Document Nothing (Just schemas) $
        JS.Schema [referenceRestriction $ substName nameSubst rootName])

    substName subst name = Y.fromMaybe name (M.lookup name subst)

    excludeAnnotatedFields = rewriteType $ \recurse typ -> case recurse typ of
        TypeRecord (RowType n fields) -> TypeRecord $ RowType n $ L.filter (not . isExcluded . fieldTypeType) fields
        TypeUnion (RowType n fields) -> TypeUnion $ RowType n $ L.filter (not . isExcluded . fieldTypeType) fields
        t -> t
      where
        isExcluded typ = Y.isJust $ getTypeAnnotation key_exclude typ

    typeToKeywordDocumentPair name typ = do
      g <- getState
      atyp <- adapterTarget <$> (withState (AdapterContext g jsonSchemaLanguage M.empty) $ termAdapter typ)
      schema <- JS.Schema <$> encodeNamedType name atyp
      return (JS.Keyword $ encodeName $ Name $ localNameOf name, schema)

    nameToPath name = namespaceToFilePath CaseConventionCamel (FileExtension "json") (Namespace $ nsPart ++ local)
      where
        (QualifiedName mns local) = qualifyName name
        nsPart = case mns of
          Nothing -> ""
          Just (Namespace ns) -> ns ++ "/"

encodeField :: FieldType -> Flow Graph (JS.Keyword, JS.Schema)
encodeField (FieldType name typ) = do
  res <- encodeType False typ
  return (JS.Keyword $ unName name, JS.Schema res)

encodeName :: Name -> String
encodeName = nonAlnumToUnderscores . unName

encodeTerm :: Term -> Flow Graph ()
encodeTerm term = fail "not yet implemented"

encodeNamedType :: Name -> Type -> Flow Graph [JS.Restriction]
encodeNamedType name typ = do
  res <- encodeType False $ stripType typ
  return $ [JS.RestrictionTitle $ unName name] ++ res

encodeType :: Bool -> Type -> Flow Graph [JS.Restriction]
encodeType optional typ = case typ of
    TypeAnnotated _ -> do
      res <- encodeType optional $ stripType typ
      mdesc <- getTypeDescription typ
      let desc = Y.maybe [] (\d -> [JS.RestrictionDescription d]) mdesc
      return $ desc ++ res
    TypeList lt -> do
      elSchema <- JS.Schema <$> encodeType False lt
      let arrayRes = [JS.RestrictionArray $ JS.ArrayRestrictionItems $ JS.ItemsSameItems elSchema]
      pure $ jsType JS.TypeNameArray ++ arrayRes
    TypeLiteral lt -> case lt of
      LiteralTypeBinary -> pure $ jsType JS.TypeNameString
      LiteralTypeBoolean -> pure $ jsType JS.TypeNameBoolean
      LiteralTypeFloat ft -> pure $ jsType JS.TypeNameNumber
      LiteralTypeInteger ft -> pure $ jsType JS.TypeNameInteger
      LiteralTypeString -> pure $ jsType JS.TypeNameString
    TypeMap (MapType _ vt) -> do -- Note: we assume that keys are strings
      vschema <- JS.Schema <$> encodeType False vt
      let objRes = [JS.RestrictionObject $ JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsSchema vschema]
      pure $ jsType JS.TypeNameObject ++ objRes
    TypeOptional t -> encodeType True t -- Note: nested optionals are lost
    TypeRecord rt -> encodeRecordOrUnion False rt
    TypeUnion rt -> if L.null simpleFields
        then asRecord rt
        else do
          recSchema <- JS.Schema <$> asRecord (rt {rowTypeFields = nonsimpleFields})
          let names = fieldTypeName <$> simpleFields
          return [JS.RestrictionMultiple $ JS.MultipleRestrictionOneOf $ [recSchema, toSimpleSchema names]]
      where
        toSimpleSchema names = JS.Schema [
          JS.RestrictionType $ JS.TypeSingle $ JS.TypeNameString,
          JS.RestrictionMultiple $ JS.MultipleRestrictionEnum (J.ValueString . unName <$> names)]
        asRecord rt = encodeRecordOrUnion True $ unionTypeToRecordType rt
        (simpleFields, nonsimpleFields) = L.partition isSimple $ rowTypeFields rt
        isSimple (FieldType _ ft) = isUnitType $ stripType ft
    TypeVariable name -> pure [referenceRestriction name]
    _ -> fail $ "unsupported type variant: " ++ show (typeVariant typ)
  where
    encodeRecordOrUnion union (RowType _ fields) = do
        props <- M.fromList <$> CM.mapM encodeField fields
        let objRes = [JS.RestrictionObject $ JS.ObjectRestrictionProperties props]
        let reqRes = if L.null reqs then [] else [JS.RestrictionObject $ JS.ObjectRestrictionRequired reqs]
        pure $ jsType JS.TypeNameObject ++ objRes ++ reqRes ++ cardRes
      where
        reqs = Y.catMaybes $ fmap ifReq fields
        ifReq field = if isRequiredField field then Just (JS.Keyword $ unName $ fieldTypeName field) else Nothing
        cardRes = JS.RestrictionObject <$>
            [JS.ObjectRestrictionAdditionalProperties $ JS.AdditionalItemsAny False]
            ++ if union then [
              JS.ObjectRestrictionMinProperties 1,
              JS.ObjectRestrictionMaxProperties 1] else []
    jsType tname = [JS.RestrictionType jst]
      where
        jst = if optional
          then JS.TypeMultiple [tname, JS.TypeNameNull]
          else JS.TypeSingle tname

isRequiredField :: FieldType -> Bool
isRequiredField (FieldType _ typ) = case stripType typ of
  TypeOptional _ -> False
  _ -> True

moduleToJsonSchemaDocuments :: JsonSchemaOptions -> Module -> Flow Graph (M.Map FilePath JS.Document)
moduleToJsonSchemaDocuments opts mod = transformModule jsonSchemaLanguage encodeTerm (constructModule opts) mod

moduleToJsonSchemaFiles :: JsonSchemaOptions -> Module -> Flow Graph (M.Map FilePath String)
moduleToJsonSchemaFiles opts mod = do
  files <- moduleToJsonSchemaDocuments opts mod
  return $ fmap jsonSchemaDocumentToString files

referenceRestriction :: Name -> JS.Restriction
referenceRestriction name = JS.RestrictionReference $ JS.SchemaReference $ "#/$defs/" ++ encodeName name
