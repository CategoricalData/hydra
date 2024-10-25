module Hydra.Ext.Json.Schema.Coder (moduleToJsonSchema) where

import Hydra.Kernel
import Hydra.TermAdapters
import Hydra.Adapters
import Hydra.Ext.Json.Schema.Language
import Hydra.Tools.Serialization
import Hydra.Ext.Json.Schema.Serde
import qualified Hydra.Ext.Json.Schema as JS
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


moduleToJsonSchema :: Module -> Flow Graph (M.Map FilePath String)
moduleToJsonSchema mod = do
    files <- moduleToDocumentMap mod
    return $ M.fromList (mapPair <$> M.toList files)
  where
    mapPair (path, doc) = (path, show doc) -- TODO

constructModule ::
  M.Map Namespace String
  -> Module
  -> M.Map Type (Coder Graph Graph Term ())
  -> [(Element, TypedTerm)]
  -> Flow Graph (M.Map FilePath JS.Document)
constructModule aliases mod coders pairs = M.fromList <$> CM.mapM toDocument pairs
  where    
    toDocument (el, TypedTerm term typ) = do
      if isType typ
        then do
          let path = nameToPath $ elementName el
          doc <- coreDecodeType term >>= encodeAdaptedType (elementName el)
          return (path, doc)
        else fail $ "mapping of non-type elements to JSON Schema is not yet supported: " ++ unName (elementName el)
        
    nameToPath name = namespaceToFilePath False (FileExtension "json") (Namespace $ nsPart ++ local)
      where
        (QualifiedName mns local) = qualifyNameLazy name
        nsPart = case mns of
          Nothing -> ""
          Just (Namespace ns) -> ns ++ "/"
          
moduleToDocumentMap :: Module -> Flow Graph (M.Map FilePath JS.Document)
moduleToDocumentMap mod = do
  aliases <- importAliasesForModule mod
  transformModule jsonSchemaLanguage (encodeTerm aliases) (constructModule aliases) mod

encodeAdaptedType :: Name -> Type -> Flow Graph JS.Document
encodeAdaptedType name typ = do
  g <- getState
  let cx = AdapterContext g jsonSchemaLanguage M.empty
  ad <- withState cx $ termAdapter typ
  encodeRootType name $ adapterTarget ad

encodeTerm :: M.Map Namespace String -> Term -> Flow Graph ()
encodeTerm aliases term = fail "not yet implemented"

encodeRootType :: Name -> Term -> Flow Graph JS.Document
encodeRootType name term = do
    typ <- coreDecodeType term
    let names = S.insert name $ typeDependencyNames typ
    depTypes <- CM.mapM requireType names
  where
    depNames = S.insert name $

encodeType :: Type -> Flow Graph JS.Document
encodeType aliases typ =



 case typ of
    TypeAnnotated (AnnotatedType typ' _) -> encodeType aliases typ'
    TypeList lt -> Left . JS.SchemaArray <$> encode lt
    TypeLiteral lt -> Left . JS.SchemaPrimitive <$> case lt of
      LiteralTypeBinary -> pure JS.PrimitiveTypeBytes
      LiteralTypeBoolean -> pure JS.PrimitiveTypeBoolean
      LiteralTypeFloat ft -> case ft of
        FloatTypeFloat32 -> pure JS.PrimitiveTypeFloat
        FloatTypeFloat64 -> pure JS.PrimitiveTypeDouble
        _ -> unexpected "float32 or float64" $ show ft
      LiteralTypeInteger it -> case it of
        IntegerTypeInt32 -> pure JS.PrimitiveTypeInt
        IntegerTypeInt64 -> pure JS.PrimitiveTypeLong
        _ -> unexpected "int32 or int64" $ show it
      LiteralTypeString -> pure JS.PrimitiveTypeString
    TypeMap (MapType kt vt) -> Left . JS.SchemaMap <$> encode vt -- note: we simply assume string as a key type
    TypeVariable name -> pure $ Left $ JS.SchemaNamed $ pdlNameForElement aliases True name
    TypeOptional ot -> fail $ "optionals unexpected at top level"
    TypeRecord rt -> do
      let includes = []
      rfields <- CM.mapM encodeRecordField $ rowTypeFields rt
      return $ Right $ JS.NamedSchema_TypeRecord $ JS.RecordSchema rfields includes
    TypeUnion rt -> if isEnum
        then do
          fs <- CM.mapM encodeEnumField $ rowTypeFields rt
          return $ Right $ JS.NamedSchema_TypeEnum $ JS.EnumSchema fs
        else Left . JS.SchemaUnion . JS.UnionSchema <$> CM.mapM encodeUnionField (rowTypeFields rt)
      where
        isEnum = L.foldl (\b t -> b && stripType t == Types.unit) True $ fmap fieldTypeType (rowTypeFields rt)
    _ -> unexpected "JS-supported type" $ show typ
  where
    encode t = case stripType t of
      TypeRecord (RowType _ []) -> encode Types.int32 -- special case for the unit type
      _ -> do
        res <- encodeType aliases t
        case res of
          Left schema -> pure schema
          Right _ -> fail $ "type resolved to an unsupported nested named schema: " ++ show t
    encodeRecordField (FieldType (Name name) typ) = do
      anns <- getAnns typ
      (schema, optional) <- encodePossiblyOptionalType typ
      return JS.RecordField {
        JS.recordFieldName = JS.FieldName name,
        JS.recordFieldValue = schema,
        JS.recordFieldOptional = optional,
        JS.recordFieldDefault = Nothing,
        JS.recordFieldAnnotations = anns}
    encodeUnionField (FieldType (Name name) typ) = do
      anns <- getAnns typ
      (s, optional) <- encodePossiblyOptionalType typ
      let schema = if optional
          then JS.SchemaUnion $ JS.UnionSchema (simpleUnionMember <$> [JS.SchemaNull, s])
          else s
      return JS.UnionMember {
        JS.unionMemberAlias = Just $ JS.FieldName name,
        JS.unionMemberValue = schema,
        JS.unionMemberAnnotations = anns}
    encodeEnumField (FieldType (Name name) typ) = do
      anns <- getAnns typ
      return JS.EnumField {
        JS.enumFieldName = JS.EnumFieldName $ convertCase CaseConventionCamel CaseConventionUpperSnake name,
        JS.enumFieldAnnotations = anns}
    encodePossiblyOptionalType typ = case stripType typ of
      TypeOptional ot -> do
        t <- encode ot
        return (t, True)
      _ -> do
        t <- encode typ
        return (t, False)
    getAnns typ = do
      r <- getTypeDescription typ
      return $ doc r

importAliasesForModule mod = do
    nss <- moduleDependencyNamespaces False True True False mod
    return $ M.fromList (toPair <$> S.toList nss)
  where
    toPair ns = (ns, slashesToDots $ unNamespace ns)

noAnnotations :: JS.Annotations
noAnnotations = JS.Annotations Nothing False

pdlNameForElement :: M.Map Namespace String -> Bool -> Name -> JS.QualifiedName
pdlNameForElement aliases withNs name = JS.QualifiedName (JS.Name local)
    $ if withNs
      then JS.Namespace <$> alias
      else Nothing
  where
    QualifiedName (Just ns) local = qualifyNameEager name
    alias = M.lookup ns aliases

jsonSchemaNameForModule :: Module -> String
jsonSchemaNameForModule = unNamespace . moduleNamespace

simpleUnionMember :: JS.Schema -> JS.UnionMember
simpleUnionMember schema = JS.UnionMember Nothing schema noAnnotations

slashesToDots :: String -> String
slashesToDots = fmap (\c -> if c == '/' then '.' else c)
