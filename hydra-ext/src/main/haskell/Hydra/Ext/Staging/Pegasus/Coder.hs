module Hydra.Ext.Staging.Pegasus.Coder (moduleToPdl) where

import Hydra.Kernel
import Hydra.Ext.Staging.Pegasus.Language
import Hydra.Ext.Staging.Pegasus.Serde
import qualified Hydra.Ext.Pegasus.Pdl as PDL
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type Result a = Either (InContext OtherError) a

err :: Context -> String -> Result a
err cx msg = Left (InContext (OtherError msg) cx)

unexpectedE :: Context -> String -> String -> Result a
unexpectedE cx expected found = err cx $ "Expected " ++ expected ++ ", found: " ++ found


moduleToPdl :: Module -> [Definition] -> Context -> Graph -> Result (M.Map FilePath String)
moduleToPdl mod defs cx g = do
    files <- moduleToPegasusSchemas cx g mod defs
    return $ M.fromList (mapPair <$> M.toList files)
  where
    mapPair (path, sf) = (path, printExpr $ parenthesize $ exprSchemaFile sf)

constructModule ::
  Context -> Graph
  -> M.Map Namespace String
  -> Module
  -> [TypeDefinition]
  -> Result (M.Map FilePath PDL.SchemaFile)
constructModule cx g aliases mod typeDefs = do
    -- Flatten the sorted groups; if any group has more than one element, it's a cycle
    let groups = topologicalSortTypeDefinitions typeDefs
    sortedDefs <- case L.find (\grp -> L.length grp > 1) groups of
      Just cycle -> err cx $ "types form a cycle (unsupported in PDL): " ++ show (typeDefinitionName <$> cycle)
      Nothing -> pure $ L.concat groups
    schemas <- CM.mapM toSchema sortedDefs
    return $ M.fromList (toPair <$> schemas)
  where
    ns = pdlNameForModule mod
    pkg = Nothing
    toPair (schema, imports) = (path, PDL.SchemaFile ns pkg imports [schema])
      where
        path = namespaceToFilePath CaseConventionCamel (FileExtension "pdl") (Namespace $ (unNamespace $ moduleNamespace mod) ++ "/" ++ local)
        local = PDL.unName $ PDL.qualifiedNameName $ PDL.namedSchemaQualifiedName schema

    toSchema typeDef = typeToSchema typeDef (typeDefinitionType typeDef)
    typeToSchema typeDef typ = do
        res <- encodeType cx g aliases typ
        let ptype = case res of
              Left schema -> PDL.NamedSchemaTypeTyperef schema
              Right t -> t
        descr <- getTypeDescription cx g typ
        let anns = doc descr
        return (PDL.NamedSchema qname ptype anns, imports)
      where
        qname = pdlNameForElement aliases False $ typeDefinitionName typeDef
        imports = []

moduleToPegasusSchemas :: Context -> Graph -> Module -> [Definition] -> Result (M.Map FilePath PDL.SchemaFile)
moduleToPegasusSchemas cx g mod defs = do
  let (typeDefs, _termDefs) = partitionDefinitions defs
  aliases <- importAliasesForModule cx g mod
  constructModule cx g aliases mod typeDefs

doc :: Y.Maybe String -> PDL.Annotations
doc s = PDL.Annotations s False

encodeType :: Context -> Graph -> M.Map Namespace String -> Type -> Result (Either PDL.Schema PDL.NamedSchemaType)
encodeType cx g aliases typ = case typ of
    TypeAnnotated (AnnotatedType typ' _) -> encodeType cx g aliases typ'
    TypeEither (EitherType lt rt) -> do
      -- Encode Either as a union with "left" and "right" variants
      leftSchema <- encode lt
      rightSchema <- encode rt
      let leftMember = PDL.UnionMember (Just $ PDL.FieldName "left") leftSchema noAnnotations
      let rightMember = PDL.UnionMember (Just $ PDL.FieldName "right") rightSchema noAnnotations
      return $ Left $ PDL.SchemaUnion $ PDL.UnionSchema [leftMember, rightMember]
    TypeList lt -> Left . PDL.SchemaArray <$> encode lt
    TypeLiteral lt -> Left . PDL.SchemaPrimitive <$> case lt of
      LiteralTypeBinary -> pure PDL.PrimitiveTypeBytes
      LiteralTypeBoolean -> pure PDL.PrimitiveTypeBoolean
      LiteralTypeFloat ft -> case ft of
        FloatTypeFloat32 -> pure PDL.PrimitiveTypeFloat
        FloatTypeFloat64 -> pure PDL.PrimitiveTypeDouble
        _ -> unexpectedE cx "float32 or float64" $ show ft
      LiteralTypeInteger it -> case it of
        IntegerTypeInt32 -> pure PDL.PrimitiveTypeInt
        IntegerTypeInt64 -> pure PDL.PrimitiveTypeLong
        _ -> unexpectedE cx "int32 or int64" $ show it
      LiteralTypeString -> pure PDL.PrimitiveTypeString
    TypeMap (MapType kt vt) -> Left . PDL.SchemaMap <$> encode vt -- note: we simply assume string as a key type
    TypePair (PairType ft st) -> do
      -- Encode Pair as a record with "first" and "second" fields
      firstSchema <- encode ft
      secondSchema <- encode st
      let firstField = PDL.RecordField (PDL.FieldName "first") firstSchema False Nothing noAnnotations
      let secondField = PDL.RecordField (PDL.FieldName "second") secondSchema False Nothing noAnnotations
      return $ Right $ PDL.NamedSchemaTypeRecord $ PDL.RecordSchema [firstField, secondField] []
    TypeSet st -> Left . PDL.SchemaArray <$> encode st  -- Encode Set as array (PDL has no native set type)
    TypeVariable name -> pure $ Left $ PDL.SchemaNamed $ pdlNameForElement aliases True name
    TypeWrap (WrappedType _ inner) -> encodeType cx g aliases inner  -- Unwrap to inner type
    TypeMaybe ot -> err cx "optionals unexpected at top level"
    TypeRecord rt -> do
      let includes = []
      rfields <- CM.mapM encodeRecordField $ rowTypeFields rt
      return $ Right $ PDL.NamedSchemaTypeRecord $ PDL.RecordSchema rfields includes
    TypeUnion rt -> if isEnum
        then do
          fs <- CM.mapM encodeEnumField $ rowTypeFields rt
          return $ Right $ PDL.NamedSchemaTypeEnum $ PDL.EnumSchema fs
        else Left . PDL.SchemaUnion . PDL.UnionSchema <$> CM.mapM encodeUnionField (rowTypeFields rt)
      where
        isEnum = L.foldl (\b t -> b && deannotateType t == Types.unit) True $ fmap fieldTypeType (rowTypeFields rt)
    _ -> unexpectedE cx "PDL-supported type" $ show typ
  where
    encode t = case deannotateType t of
      TypeRecord (RowType _ []) -> encode Types.int32 -- special case for the unit type
      _ -> do
        res <- encodeType cx g aliases t
        case res of
          Left schema -> pure schema
          Right _ -> err cx $ "type resolved to an unsupported nested named schema: " ++ show t
    encodeRecordField (FieldType (Name name) typ) = do
      anns <- getAnns typ
      (schema, optional) <- encodePossiblyOptionalType typ
      return PDL.RecordField {
        PDL.recordFieldName = PDL.FieldName name,
        PDL.recordFieldValue = schema,
        PDL.recordFieldOptional = optional,
        PDL.recordFieldDefault = Nothing,
        PDL.recordFieldAnnotations = anns}
    encodeUnionField (FieldType (Name name) typ) = do
      anns <- getAnns typ
      (s, optional) <- encodePossiblyOptionalType typ
      let schema = if optional
          then PDL.SchemaUnion $ PDL.UnionSchema (simpleUnionMember <$> [PDL.SchemaNull, s])
          else s
      return PDL.UnionMember {
        PDL.unionMemberAlias = Just $ PDL.FieldName name,
        PDL.unionMemberValue = schema,
        PDL.unionMemberAnnotations = anns}
    encodeEnumField (FieldType (Name name) typ) = do
      anns <- getAnns typ
      return PDL.EnumField {
        PDL.enumFieldName = PDL.EnumFieldName $ convertCase CaseConventionCamel CaseConventionUpperSnake name,
        PDL.enumFieldAnnotations = anns}
    encodePossiblyOptionalType typ = case deannotateType typ of
      TypeMaybe ot -> do
        t <- encode ot
        return (t, True)
      _ -> do
        t <- encode typ
        return (t, False)
    getAnns typ = do
      r <- getTypeDescription cx g typ
      return $ doc r

importAliasesForModule :: Context -> Graph -> Module -> Result (M.Map Namespace String)
importAliasesForModule cx g mod = do
    nss <- moduleDependencyNamespaces cx g False True True False mod
    return $ M.fromList (toPair <$> S.toList nss)
  where
    toPair ns = (ns, slashesToDots $ unNamespace ns)

noAnnotations :: PDL.Annotations
noAnnotations = PDL.Annotations Nothing False

pdlNameForElement :: M.Map Namespace String -> Bool -> Name -> PDL.QualifiedName
pdlNameForElement aliases withNs name = PDL.QualifiedName (PDL.Name local)
    $ if withNs
      then PDL.Namespace <$> alias
      else Nothing
  where
    QualifiedName (Just ns) local = qualifyName name
    alias = M.lookup ns aliases

pdlNameForModule :: Module -> PDL.Namespace
pdlNameForModule = PDL.Namespace . slashesToDots . h . moduleNamespace
  where
    h (Namespace n) = n

simpleUnionMember :: PDL.Schema -> PDL.UnionMember
simpleUnionMember schema = PDL.UnionMember Nothing schema noAnnotations

slashesToDots :: String -> String
slashesToDots = fmap (\c -> if c == '/' then '.' else c)
