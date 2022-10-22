module Hydra.Ext.Pegasus.Coder (printModule) where

import Hydra.Adapters.Term
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.CoreLanguage
import Hydra.Compute
import Hydra.Module
import Hydra.Monads
import Hydra.Rewriting
import Hydra.Adapters.Coders
import Hydra.Util.Formatting
import Hydra.Ext.Pegasus.Language
import qualified Hydra.Ext.Pegasus.Pdl as PDL
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Util.Codetree.Script
import Hydra.Ext.Pegasus.Serde

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


printModule :: (Ord m, Read m, Show m) => Module m -> GraphFlow m (M.Map FilePath String)
printModule mod = do
    files <- moduleToPegasusSchemas mod
    return $ M.fromList (mapPair <$> M.toList files)
  where
    mapPair (path, sf) = (path, printExpr $ parenthesize $ exprSchemaFile sf)

constructModule :: (Ord m, Read m, Show m)
  => Module m
  -> M.Map (Type m) (Coder (Context m) (Context m) (Term m) ())
  -> [(Element m, TypedTerm m)]
  -> GraphFlow m (M.Map FilePath PDL.SchemaFile)
constructModule mod coders pairs = do
    sortedPairs <- case (topologicalSortElements $ fst <$> pairs) of
      Nothing -> fail $ "types form a cycle (unsupported in PDL)"
      Just sorted -> pure $ Y.catMaybes $ fmap (\n -> M.lookup n pairByName) sorted
    schemas <- CM.mapM toSchema sortedPairs
    return $ M.fromList (toPair <$> schemas)
  where
    ns = pdlNameForModule mod
    pkg = Nothing
    imports = [] -- TODO
    toPair schema = (path, PDL.SchemaFile ns pkg imports [schema])
      where
        path = namespaceToFilePath False (FileExtension "pdl") (Namespace $ (unNamespace $ moduleNamespace mod) ++ "/" ++ local)
        local = PDL.unName $ PDL.qualifiedNameName $ PDL.namedSchemaQualifiedName schema
    pairByName = L.foldl (\m p -> M.insert (elementName $ fst p) p m) M.empty pairs
    aliases = importAliasesForModule mod
    toSchema (el, TypedTerm typ term) = do
      cx <- getState
      if isType cx typ
        then decodeType term >>= typeToSchema el
        else fail $ "mapping of non-type elements to PDL is not yet supported: " ++ unName (elementName el)
    typeToSchema el typ = do
      let qname = pdlNameForElement aliases False $ elementName el
      res <- encodeAdaptedType aliases typ
      let ptype = case res of
            Left schema -> PDL.NamedSchema_TypeTyperef schema
            Right t -> t
      cx <- getState
      r <- annotationClassTermDescription (contextAnnotations cx) $ elementData el
      let anns = doc r
      return $ PDL.NamedSchema qname ptype anns

moduleToPegasusSchemas :: (Ord m, Read m, Show m) => Module m -> GraphFlow m (M.Map FilePath PDL.SchemaFile)
moduleToPegasusSchemas mod = transformModule language (encodeTerm aliases) constructModule mod
  where
    aliases = importAliasesForModule mod

doc :: Y.Maybe String -> PDL.Annotations
doc s = PDL.Annotations s False

encodeAdaptedType :: (Ord m, Read m, Show m)
  => M.Map Namespace String -> Type m
  -> GraphFlow m (Either PDL.Schema PDL.NamedSchema_Type)
encodeAdaptedType aliases typ = do
  cx <- getState
  let acx = AdapterContext cx hydraCoreLanguage language
  ad <- withState acx $ termAdapter typ
  encodeType aliases $ adapterTarget ad

encodeTerm :: (Eq m, Ord m, Read m, Show m) => M.Map Namespace String -> Term m -> GraphFlow m ()
encodeTerm aliases term = fail "not yet implemented"

encodeType :: (Eq m, Show m) => M.Map Namespace String -> Type m -> GraphFlow m (Either PDL.Schema PDL.NamedSchema_Type)
encodeType aliases typ = case typ of
    TypeAnnotated (Annotated typ' _) -> encodeType aliases typ'
    TypeElement et -> pure $ Left $ PDL.SchemaPrimitive PDL.PrimitiveTypeString
    TypeList lt -> Left . PDL.SchemaArray <$> encode lt
    TypeLiteral lt -> Left . PDL.SchemaPrimitive <$> case lt of
      LiteralTypeBinary -> pure PDL.PrimitiveTypeBytes
      LiteralTypeBoolean -> pure PDL.PrimitiveTypeBoolean
      LiteralTypeFloat ft -> case ft of
        FloatTypeFloat32 -> pure PDL.PrimitiveTypeFloat
        FloatTypeFloat64 -> pure PDL.PrimitiveTypeDouble
        _ -> unexpected "float32 or float64" ft
      LiteralTypeInteger it -> case it of
        IntegerTypeInt32 -> pure PDL.PrimitiveTypeInt
        IntegerTypeInt64 -> pure PDL.PrimitiveTypeLong
        _ -> unexpected "int32 or int64" it
      LiteralTypeString -> pure PDL.PrimitiveTypeString
    TypeMap (MapType kt vt) -> Left . PDL.SchemaMap <$> encode vt -- note: we simply assume string as a key type
    TypeNominal name -> pure $ Left $ PDL.SchemaNamed $ pdlNameForElement aliases True name
    TypeOptional ot -> fail $ "optionals unexpected at top level"
    TypeRecord rt -> do
      let includes = []
      rfields <- CM.mapM encodeRecordField $ rowTypeFields rt
      return $ Right $ PDL.NamedSchema_TypeRecord $ PDL.RecordSchema rfields includes
    TypeUnion rt -> if isEnum
        then do
          fs <- CM.mapM encodeEnumField $ rowTypeFields rt
          return $ Right $ PDL.NamedSchema_TypeEnum $ PDL.EnumSchema fs
        else Left . PDL.SchemaUnion . PDL.UnionSchema <$> CM.mapM encodeUnionField (rowTypeFields rt)
      where
        isEnum = L.foldl (\b t -> b && stripType t == Types.unit) True $ fmap fieldTypeType (rowTypeFields rt)
    _ -> unexpected "PDL-supported type" typ
  where
    encode t = case stripType t of
      TypeRecord (RowType _ Nothing []) -> encode Types.int32 -- special case for the unit type
      _ -> do
        res <- encodeType aliases t
        case res of
          Left schema -> pure schema
          Right _ -> fail $ "type resolved to an unsupported nested named schema: " ++ show t
    encodeRecordField (FieldType (FieldName name) typ) = do
      anns <- getAnns typ
      (schema, optional) <- encodePossiblyOptionalType typ
      return PDL.RecordField {
        PDL.recordFieldName = PDL.FieldName name,
        PDL.recordFieldValue = schema,
        PDL.recordFieldOptional = optional,
        PDL.recordFieldDefault = Nothing,
        PDL.recordFieldAnnotations = anns}
    encodeUnionField (FieldType (FieldName name) typ) = do
      anns <- getAnns typ
      (s, optional) <- encodePossiblyOptionalType typ
      let schema = if optional
          then PDL.SchemaUnion $ PDL.UnionSchema (simpleUnionMember <$> [PDL.SchemaNull, s])
          else s
      return PDL.UnionMember {
        PDL.unionMemberAlias = Just $ PDL.FieldName name,
        PDL.unionMemberValue = schema,
        PDL.unionMemberAnnotations = anns}
    encodeEnumField (FieldType (FieldName name) typ) = do
      anns <- getAnns typ
      return PDL.EnumField {
        PDL.enumFieldName = PDL.EnumFieldName $ convertCase CaseCamel CaseUpperSnake name,
        PDL.enumFieldAnnotations = anns}
    encodePossiblyOptionalType typ = case stripType typ of
      TypeOptional ot -> do
        t <- encode ot
        return (t, True)
      _ -> do
        t <- encode typ
        return (t, False)
    getAnns typ = do
      cx <- getState
      r <- annotationClassTypeDescription (contextAnnotations cx) typ
      return $ doc r

importAliasesForModule g = M.empty -- TODO

noAnnotations :: PDL.Annotations
noAnnotations = PDL.Annotations Nothing False

pdlNameForElement :: M.Map Namespace String -> Bool -> Name -> PDL.QualifiedName
pdlNameForElement aliases withNs name = PDL.QualifiedName (PDL.Name local)
    $ if withNs
      then PDL.Namespace . slashesToDots <$> alias
      else Nothing
  where
    (ns, local) = toQnameEager name
    alias = M.lookup ns aliases

pdlNameForModule :: Module m -> PDL.Namespace
pdlNameForModule = PDL.Namespace . slashesToDots . h . moduleNamespace
  where
    h (Namespace n) = n

simpleUnionMember :: PDL.Schema -> PDL.UnionMember
simpleUnionMember schema = PDL.UnionMember Nothing schema noAnnotations

slashesToDots :: String -> String
slashesToDots = fmap (\c -> if c == '/' then '.' else c)
