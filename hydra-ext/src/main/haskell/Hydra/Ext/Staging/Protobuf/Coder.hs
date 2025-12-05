module Hydra.Ext.Staging.Protobuf.Coder (moduleToProtobuf) where

import Hydra.Kernel
import Hydra.Ext.Protobuf.Language
import qualified Hydra.Ext.Protobuf.Proto3 as P3
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import Hydra.Ext.Staging.Protobuf.Serde
import Hydra.Ext.Staging.CoderUtils (partititonDefinitions)
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.Annotations
import qualified Hydra.Extract.Core as ExtractCore

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


key_proto_field_index = Name "proto_field_index"

-- Track structural types (Either, Pair) that need helper message definitions
data StructuralTypeRef
  = StructuralTypeRefEither Type Type
  | StructuralTypeRefPair Type Type
  deriving (Eq, Ord, Show)

-- Generate a message name for a structural type reference
structuralTypeName :: Namespace -> StructuralTypeRef -> P3.TypeName
structuralTypeName localNs ref = P3.TypeName $ case ref of
  StructuralTypeRefEither lt rt -> "Either_" ++ typeSuffix lt ++ "_" ++ typeSuffix rt
  StructuralTypeRefPair ft st -> "Pair_" ++ typeSuffix ft ++ "_" ++ typeSuffix st
  where
    typeSuffix typ = case simplifyType typ of
      TypeLiteral lt -> case lt of
        LiteralTypeBinary -> "bytes"
        LiteralTypeBoolean -> "bool"
        LiteralTypeFloat ft -> case ft of
          FloatTypeFloat32 -> "float"
          FloatTypeFloat64 -> "double"
          _ -> "float"
        LiteralTypeInteger it -> case it of
          IntegerTypeInt32 -> "int32"
          IntegerTypeInt64 -> "int64"
          IntegerTypeUint32 -> "uint32"
          IntegerTypeUint64 -> "uint64"
          _ -> "int64"
        LiteralTypeString -> "string"
      TypeRecord (RowType name _) -> localNameOf name
      TypeUnion (RowType name _) -> localNameOf name
      TypeVariable name -> localNameOf name
      TypeUnit -> "unit"
      TypeList _ -> "list"
      TypeSet _ -> "set"
      TypeMap _ -> "map"
      TypeMaybe _ -> "maybe"
      _ -> "value"

-- Generate a helper message definition for a structural type
generateStructuralTypeMessage :: Namespace -> StructuralTypeRef -> Flow Graph P3.Definition
generateStructuralTypeMessage localNs ref = do
  resetCount key_proto_field_index
  nextIndex
  case ref of
    StructuralTypeRefEither lt rt -> do
      leftField <- makeField localNs "left" lt
      rightField <- makeField localNs "right" rt
      return $ P3.DefinitionMessage P3.MessageDefinition {
        P3.messageDefinitionName = structuralTypeName localNs ref,
        P3.messageDefinitionFields = [leftField, rightField],
        P3.messageDefinitionOptions = []}
    StructuralTypeRefPair ft st -> do
      firstField <- makeField localNs "first" ft
      secondField <- makeField localNs "second" st
      return $ P3.DefinitionMessage P3.MessageDefinition {
        P3.messageDefinitionName = structuralTypeName localNs ref,
        P3.messageDefinitionFields = [firstField, secondField],
        P3.messageDefinitionOptions = []}
  where
    makeField ns fname ftyp = do
      ft <- encodeSimpleTypeForHelper ns ftyp
      idx <- nextIndex
      return $ P3.Field {
        P3.fieldName = P3.FieldName fname,
        P3.fieldJsonName = Nothing,
        P3.fieldType = P3.FieldTypeSimple ft,
        P3.fieldNumber = idx,
        P3.fieldOptions = []}

-- Encode a simple type for helper message fields
encodeSimpleTypeForHelper :: Namespace -> Type -> Flow Graph P3.SimpleType
encodeSimpleTypeForHelper localNs typ = case simplifyType typ of
  TypeLiteral lt -> P3.SimpleTypeScalar <$> encodeScalarType lt
  TypeRecord (RowType name _) -> forNominal name
  TypeUnion (RowType name _) -> forNominal name
  TypeUnit -> pure $ P3.SimpleTypeReference $ P3.TypeName $ "google.protobuf.Empty"
  TypeVariable name -> forNominal name
  t -> unexpected "simple type in structural type helper" $ show $ removeTypeAnnotations t
  where
    forNominal name = pure $ P3.SimpleTypeReference $ encodeTypeReference localNs name

-- Collect all structural type references (Either, Pair) from a list of types
collectStructuralTypes :: [Type] -> S.Set StructuralTypeRef
collectStructuralTypes types = L.foldl S.union S.empty (collectFromType <$> types)
  where
    collectFromType = foldOverType TraversalOrderPre collect S.empty
    collect acc typ = case simplifyType typ of
      TypeEither (EitherType lt rt) -> S.insert (StructuralTypeRefEither lt rt) acc
      TypePair (PairType ft st) -> S.insert (StructuralTypeRefPair ft st) acc
      _ -> acc

-- | Note: follows the Protobuf Style Guide (https://protobuf.dev/programming-guides/style)
moduleToProtobuf :: Module -> [Definition] -> Flow Graph (M.Map FilePath String)
moduleToProtobuf mod defs = withTrace ("encode module to Protobuf: " ++ unNamespace ns) $ do
    let (typeDefs, _termDefs) = partititonDefinitions defs
    pfile <- constructModule mod typeDefs
    let content = printExpr $ parenthesize $ writeProtoFile pfile
    return $ M.fromList [(path, content)]
  where
    ns = moduleNamespace mod
    path = P3.unFileReference $ namespaceToFileReference ns

--

javaMultipleFilesOptionName = "java_multiple_files"
javaPackageOptionName = "java_package"

constructModule :: Module -> [TypeDefinition] -> Flow Graph P3.ProtoFile
constructModule mod@(Module ns _ _ _ desc) typeDefs = do
    schemaImports <- (fmap namespaceToFileReference . S.toList) <$> moduleDependencyNamespaces True False False False mod
    definitions <- CM.mapM toDef typeDefs
    let types = typeDefinitionType <$> typeDefs
    -- Collect structural type references and generate helper messages
    let structRefs = collectStructuralTypes types
    helperDefs <- CM.mapM (generateStructuralTypeMessage ns) (S.toList structRefs)
    return P3.ProtoFile {
      P3.protoFilePackage = namespaceToPackageName ns,
      P3.protoFileImports = schemaImports ++ wrapperImport types ++ emptyImport types,
      P3.protoFileTypes = helperDefs ++ definitions,
      P3.protoFileOptions = descOption:javaOptions}
  where
    javaOptions = [
      P3.Option javaMultipleFilesOptionName $ P3.ValueBoolean True,
      P3.Option javaPackageOptionName $ P3.ValueString $ P3.unPackageName $ namespaceToPackageName ns]
    descOption = P3.Option descriptionOptionName $ P3.ValueString $
      (Y.maybe "" (\d -> d ++ "\n\n") desc) ++ warningAutoGeneratedFile
    toDef (TypeDefinition name typ) =
      adaptTypeToLanguageAndEncode protobufLanguage (encodeDefinition ns name) $ flattenType typ
    checkFields checkType checkFieldType types = L.foldl (||) False (hasMatches <$> types)
      where
        hasMatches = foldOverType TraversalOrderPre (\b t -> b || hasMatch t) False
        hasMatch typ = case checkType typ of
          Just b -> b
          Nothing -> case typ of
            TypeRecord rt -> checkRowType rt
            TypeUnion rt -> checkRowType rt
            _ -> False
        checkRowType (RowType _ fields) = L.foldl (||) False (checkField <$> fields)
        checkField (FieldType _ typ) = checkFieldType $ deannotateType typ
    wrapperImport types = if checkFields (const Nothing) isOptionalScalarField types
        then [P3.FileReference "google/protobuf/wrappers.proto"]
        else []
      where
        isOptionalScalarField typ = case typ of
          TypeMaybe ot -> case deannotateType ot of
            TypeLiteral _ -> True
            _ -> False
          _ -> False
    emptyImport types = if checkFields checkType EncodeCore.isUnitType types
        then [P3.FileReference "google/protobuf/empty.proto"]
        else []
      where
        checkType typ = if isEnumDefinition typ
          then Just False
          else Nothing

encodeDefinition :: Namespace -> Name -> Type -> Flow Graph P3.Definition
encodeDefinition localNs name typ = withTrace ("encoding " ++ unName name) $ do
    resetCount key_proto_field_index
    nextIndex
    options <- findOptions typ
    encode options typ
  where
    wrapAsRecordType t = TypeRecord $ RowType name [FieldType (Name "value") t]
    encode options typ = case simplifyType typ of
      TypeRecord rt -> P3.DefinitionMessage <$> encodeRecordType localNs options rt
      TypeUnion rt -> if isEnumDefinition typ
        then P3.DefinitionEnum <$> encodeEnumDefinition options rt
        else encode options $ wrapAsRecordType $ TypeUnion rt
      t -> encode options $ wrapAsRecordType t

encodeEnumDefinition :: [P3.Option] -> RowType -> Flow Graph P3.EnumDefinition
encodeEnumDefinition options (RowType tname fields) = do
    values <- CM.zipWithM encodeEnumField fields [1..]
    return $ P3.EnumDefinition {
      P3.enumDefinitionName = encodeTypeName tname,
      P3.enumDefinitionValues = unspecifiedField:values,
      P3.enumDefinitionOptions = options}
  where
    unspecifiedField = P3.EnumValue {
      P3.enumValueName = encodeEnumValueName tname $ Name "unspecified",
      P3.enumValueNumber = 0,
      P3.enumValueOptions = []}
    encodeEnumField (FieldType fname ftype) idx = do
      opts <- findOptions ftype
      return $ P3.EnumValue {
        P3.enumValueName = encodeEnumValueName tname fname,
        P3.enumValueNumber = idx,
        P3.enumValueOptions = opts}

encodeEnumValueName :: Name -> Name -> P3.EnumValueName
encodeEnumValueName tname fname = P3.EnumValueName (prefix ++ "_" ++ suffix)
  where
    prefix = nonAlnumToUnderscores $ convertCaseCamelToUpperSnake $ localNameOf tname
    suffix = nonAlnumToUnderscores $ convertCaseCamelToUpperSnake $ unName fname

encodeFieldName :: Bool -> Name -> P3.FieldName
encodeFieldName preserve = P3.FieldName . toPname . unName
  where
    toPname = if preserve
      then id
      else convertCaseCamelToLowerSnake

encodeFieldType :: Namespace -> FieldType -> Flow Graph P3.Field
encodeFieldType localNs (FieldType fname ftype) = withTrace ("encode field " ++ show (unName fname)) $ do
    options <- findOptions ftype
    ft <- encodeType ftype
    idx <- nextIndex
    preserve <- readBooleanAnnotation key_preserveFieldName ftype
    return $ P3.Field {
      P3.fieldName = encodeFieldName preserve fname,
      P3.fieldJsonName = Nothing,
      P3.fieldType = ft,
      P3.fieldNumber = idx,
      P3.fieldOptions = options}
  where
    encodeType typ = case simplifyType typ of
      TypeEither (EitherType lt rt) -> do
        -- Reference the generated helper message type for this Either instantiation
        let ref = StructuralTypeRefEither lt rt
        return $ P3.FieldTypeSimple $ P3.SimpleTypeReference $ structuralTypeName localNs ref
      TypePair (PairType ft st) -> do
        -- Reference the generated helper message type for this Pair instantiation
        let ref = StructuralTypeRefPair ft st
        return $ P3.FieldTypeSimple $ P3.SimpleTypeReference $ structuralTypeName localNs ref
      TypeList lt -> do
        P3.FieldTypeRepeated <$> encodeSimpleType True lt
      TypeSet st -> do
        -- Encode Set as a repeated field (same as List)
        P3.FieldTypeRepeated <$> encodeSimpleType True st
      TypeMap (MapType kt vt) -> P3.FieldTypeMap <$> (P3.MapType <$> encodeSimpleType False kt <*> encodeSimpleType True vt)
      TypeMaybe ot -> case deannotateType ot of
        TypeLiteral lt -> P3.FieldTypeSimple <$> encodeScalarTypeWrapped lt
        _ -> encodeType ot -- TODO
      TypeUnion (RowType _ fields) -> do
        pfields <- CM.mapM (encodeFieldType localNs) fields
        return $ P3.FieldTypeOneof pfields
      _ -> do
        P3.FieldTypeSimple <$> encodeSimpleType True typ
    encodeSimpleType noms typ = case simplifyType typ of
        TypeLiteral lt -> P3.SimpleTypeScalar <$> encodeScalarType lt
        TypeRecord (RowType name _) -> forNominal name
        TypeUnion (RowType name _) -> forNominal name
        TypeUnit -> pure $ P3.SimpleTypeReference $ P3.TypeName $ "google.protobuf.Empty"
        TypeVariable name -> if noms
          then forNominal name
          else do
            typ <- (bindingTerm <$> requireElement name) >>= DecodeCore.type_
            encodeSimpleType noms typ
        t -> unexpected "simple type" $ show $ removeTypeAnnotations t
      where
        forNominal name = pure $ P3.SimpleTypeReference $ encodeTypeReference localNs name

encodeRecordType :: Namespace -> [P3.Option] -> RowType -> Flow Graph P3.MessageDefinition
encodeRecordType localNs options (RowType tname fields) = do
    pfields <- CM.mapM (encodeFieldType localNs) fields
    return P3.MessageDefinition {
      P3.messageDefinitionName = encodeTypeName tname,
      P3.messageDefinitionFields = pfields,
      P3.messageDefinitionOptions = options}

encodeScalarType :: LiteralType -> Flow s P3.ScalarType
encodeScalarType lt = case lt of
  LiteralTypeBinary -> return P3.ScalarTypeBytes
  LiteralTypeBoolean -> return P3.ScalarTypeBool
  LiteralTypeFloat ft -> case ft of
    FloatTypeFloat32 -> return P3.ScalarTypeFloat
    FloatTypeFloat64 -> return P3.ScalarTypeDouble
    _ -> unexpected "32-bit or 64-bit floating-point type" $ show ft
  LiteralTypeInteger it -> case it of
    IntegerTypeInt32 -> return P3.ScalarTypeInt32
    IntegerTypeInt64 -> return P3.ScalarTypeInt64
    IntegerTypeUint32 -> return P3.ScalarTypeUint32
    IntegerTypeUint64 -> return P3.ScalarTypeUint64
    _ -> unexpected "32-bit or 64-bit integer type" $ show it
  LiteralTypeString -> return P3.ScalarTypeString

encodeScalarTypeWrapped :: LiteralType -> Flow s P3.SimpleType
encodeScalarTypeWrapped lt = toType <$> case lt of
    LiteralTypeBinary -> return "Bytes"
    LiteralTypeBoolean -> return "Bool"
    LiteralTypeFloat ft -> case ft of
      FloatTypeFloat32 -> return "Float"
      FloatTypeFloat64 -> return "Double"
      _ -> unexpected "32-bit or 64-bit floating-point type" $ show ft
    LiteralTypeInteger it -> case it of
      IntegerTypeInt32 -> return "Int32"
      IntegerTypeInt64 -> return "Int64"
      IntegerTypeUint32 -> return "UInt32"
      IntegerTypeUint64 -> return "UInt64"
      _ -> unexpected "32-bit or 64-bit integer type" $ show it
    LiteralTypeString -> return "String"
  where
    toType label = P3.SimpleTypeReference $ P3.TypeName $ "google.protobuf." ++ label ++ "Value"

encodeTypeName :: Name -> P3.TypeName
encodeTypeName = P3.TypeName . localNameOf

encodeTypeReference :: Namespace -> Name -> P3.TypeName
encodeTypeReference localNs name = P3.TypeName $ if nsParts == Just localNsParts
    then local
    else case nsParts of
      Nothing -> local
      Just parts -> L.intercalate "." (parts ++ [local])
  where
    QualifiedName ns local = qualifyName name
    nsParts = fmap (\n -> L.init $ Strings.splitOn "." $ unNamespace n) ns
    localNsParts = L.init $ Strings.splitOn "." $ unNamespace localNs

-- Eliminate type lambdas and type applications, simply replacing type variables with the string type
flattenType :: Type -> Type
flattenType = rewriteType f
  where
   f recurse typ = case typ of
     TypeForall (ForallType v body) -> recurse $ replaceFreeTypeVariable v Types.string body
     TypeApplication (ApplicationType lhs _) -> recurse lhs
     _ -> recurse typ

findOptions :: Type -> Flow Graph [P3.Option]
findOptions typ = do
  mdesc <- getTypeDescription typ
  bdep <- readBooleanAnnotation key_deprecated typ
  let mdescAnn = fmap (\desc -> P3.Option descriptionOptionName $ P3.ValueString desc) mdesc
  let mdepAnn = if bdep then Just (P3.Option deprecatedOptionName $ P3.ValueBoolean True) else Nothing
  return $ Y.catMaybes [mdescAnn, mdepAnn]

isEnumFields :: [FieldType] -> Bool
isEnumFields fields = L.foldl (&&) True $ fmap isEnumField fields
  where
    isEnumField = EncodeCore.isUnitType . simplifyType . fieldTypeType

isEnumDefinition :: Type -> Bool
isEnumDefinition typ = case simplifyType typ of
  TypeUnion (RowType _ fields) -> isEnumFields fields
  _ -> False

namespaceToFileReference :: Namespace -> P3.FileReference
namespaceToFileReference (Namespace ns) = P3.FileReference $ pns ++ ".proto"
  where
    pns = Strings.intercalate "/" (convertCaseCamelToLowerSnake <$> (Strings.splitOn "." ns))

namespaceToPackageName :: Namespace -> P3.PackageName
namespaceToPackageName (Namespace ns) = P3.PackageName $ Strings.intercalate "." $
  convertCaseCamelToLowerSnake <$> (L.init $ Strings.splitOn "." ns)

nextIndex :: Flow s Int
nextIndex = nextCount key_proto_field_index

readBooleanAnnotation :: Name -> Type -> Flow Graph Bool
readBooleanAnnotation key typ = case M.lookup key (typeAnnotationInternal typ) of
  Nothing -> return False
  Just term -> ExtractCore.boolean term

-- Note: this should probably be done in the term adapters
simplifyType :: Type -> Type
simplifyType typ = case deannotateType typ of
  TypeWrap (WrappedType _ t) -> simplifyType t
  t -> t
