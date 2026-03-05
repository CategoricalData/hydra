module Hydra.Ext.Staging.Protobuf.Coder (moduleToProtobuf) where

import Hydra.Kernel
import Hydra.Ext.Protobuf.Language
import qualified Hydra.Ext.Protobuf.Proto3 as P3
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Schemas as Schemas
import Hydra.Ext.Staging.Protobuf.Serde
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Util as Util

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

fromEitherString :: Context -> Either String a -> Result a
fromEitherString cx (Left msg) = err cx msg
fromEitherString _ (Right a) = Right a


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
-- Returns the definition and the updated context (counter state)
generateStructuralTypeMessage :: Context -> Graph -> Namespace -> StructuralTypeRef -> Result (P3.Definition, Context)
generateStructuralTypeMessage cx g localNs ref = do
  let cx1 = resetCount key_proto_field_index cx
  let (_, cx2) = nextCount key_proto_field_index cx1
  case ref of
    StructuralTypeRefEither lt rt -> do
      (leftField, cx3) <- makeField cx2 localNs "left" lt
      (rightField, cx4) <- makeField cx3 localNs "right" rt
      return (P3.DefinitionMessage P3.MessageDefinition {
        P3.messageDefinitionName = structuralTypeName localNs ref,
        P3.messageDefinitionFields = [leftField, rightField],
        P3.messageDefinitionOptions = []}, cx4)
    StructuralTypeRefPair ft st -> do
      (firstField, cx3) <- makeField cx2 localNs "first" ft
      (secondField, cx4) <- makeField cx3 localNs "second" st
      return (P3.DefinitionMessage P3.MessageDefinition {
        P3.messageDefinitionName = structuralTypeName localNs ref,
        P3.messageDefinitionFields = [firstField, secondField],
        P3.messageDefinitionOptions = []}, cx4)
  where
    makeField cx0 ns fname ftyp = do
      ft <- encodeSimpleTypeForHelper cx0 localNs ftyp
      let (idx, cx1) = nextCount key_proto_field_index cx0
      return (P3.Field {
        P3.fieldName = P3.FieldName fname,
        P3.fieldJsonName = Nothing,
        P3.fieldType = P3.FieldTypeSimple ft,
        P3.fieldNumber = idx,
        P3.fieldOptions = []}, cx1)

-- Encode a simple type for helper message fields
encodeSimpleTypeForHelper :: Context -> Namespace -> Type -> Result P3.SimpleType
encodeSimpleTypeForHelper cx localNs typ = case simplifyType typ of
  TypeLiteral lt -> P3.SimpleTypeScalar <$> encodeScalarType cx lt
  TypeRecord (RowType name _) -> forNominal name
  TypeUnion (RowType name _) -> forNominal name
  TypeUnit -> pure $ P3.SimpleTypeReference $ P3.TypeName $ "google.protobuf.Empty"
  TypeVariable name -> forNominal name
  t -> unexpectedE cx "simple type in structural type helper" $ show $ removeTypeAnnotations t
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
moduleToProtobuf :: Module -> [Definition] -> Context -> Graph -> Result (M.Map FilePath String)
moduleToProtobuf mod defs cx g = do
    let (typeDefs, _termDefs) = partitionDefinitions defs
    pfile <- constructModule cx g mod typeDefs
    let content = printExpr $ parenthesize $ writeProtoFile pfile
    return $ M.fromList [(path, content)]
  where
    ns = moduleNamespace mod
    path = P3.unFileReference $ namespaceToFileReference ns

--

javaMultipleFilesOptionName = "java_multiple_files"
javaPackageOptionName = "java_package"

constructModule :: Context -> Graph -> Module -> [TypeDefinition] -> Result P3.ProtoFile
constructModule cx g mod@(Module ns _ _ _ desc) typeDefs = do
    schemaImports <- (fmap namespaceToFileReference . S.toList) <$> moduleDependencyNamespaces cx g True False False False mod
    let encodeDefEither name typ = encodeDefinition cx g ns name typ
    let toDef (TypeDefinition name typ) =
          fromEitherString cx $ adaptTypeToLanguageAndEncode protobufLanguage (encodeDefEither name) cx g $ flattenType typ
    definitions <- CM.mapM toDef typeDefs
    let types = typeDefinitionType <$> typeDefs
    -- Collect structural type references and generate helper messages
    let structRefs = collectStructuralTypes types
    (helperDefs, _cx') <- mapAccumResult (generateStructuralTypeMessage' ns) cx (S.toList structRefs)
    return P3.ProtoFile {
      P3.protoFilePackage = namespaceToPackageName ns,
      P3.protoFileImports = schemaImports ++ wrapperImport types ++ emptyImport types,
      P3.protoFileTypes = helperDefs ++ definitions,
      P3.protoFileOptions = descOption:javaOptions}
  where
    generateStructuralTypeMessage' ns0 cx0 ref = generateStructuralTypeMessage cx0 g ns0 ref
    javaOptions = [
      P3.Option javaMultipleFilesOptionName $ P3.ValueBoolean True,
      P3.Option javaPackageOptionName $ P3.ValueString $ P3.unPackageName $ namespaceToPackageName ns]
    descOption = P3.Option descriptionOptionName $ P3.ValueString $
      (Y.maybe "" (\d -> d ++ "\n\n") desc) ++ warningAutoGeneratedFile
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
    emptyImport types = if checkFields checkType Schemas.isUnitType types
        then [P3.FileReference "google/protobuf/empty.proto"]
        else []
      where
        checkType typ = if isEnumDefinition typ
          then Just False
          else Nothing

-- Helper to thread context through a list, accumulating results
mapAccumResult :: (cx -> a -> Result (b, cx)) -> cx -> [a] -> Result ([b], cx)
mapAccumResult _ cx0 [] = Right ([], cx0)
mapAccumResult f cx0 (x:xs) = do
  (b, cx1) <- f cx0 x
  (bs, cx2) <- mapAccumResult f cx1 xs
  return (b:bs, cx2)

encodeDefinition :: Context -> Graph -> Namespace -> Name -> Type -> Either String P3.Definition
encodeDefinition cx g localNs name typ = do
    let cx1 = resetCount key_proto_field_index cx
    let (_, cx2) = nextCount key_proto_field_index cx1
    options <- toEitherString $ findOptions cx g typ
    toEitherString $ encode cx2 options typ
  where
    wrapAsRecordType t = TypeRecord $ RowType name [FieldType (Name "value") t]
    encode cx0 options typ = case simplifyType typ of
      TypeRecord rt -> P3.DefinitionMessage <$> encodeRecordType cx0 g localNs options rt
      TypeUnion rt -> if isEnumDefinition typ
        then P3.DefinitionEnum <$> encodeEnumDefinition cx0 g options rt
        else encode cx0 options $ wrapAsRecordType $ TypeUnion rt
      t -> encode cx0 options $ wrapAsRecordType t
    toEitherString :: Result a -> Either String a
    toEitherString (Right a) = Right a
    toEitherString (Left (InContext (OtherError msg) _)) = Left msg

encodeEnumDefinition :: Context -> Graph -> [P3.Option] -> RowType -> Result P3.EnumDefinition
encodeEnumDefinition cx g options (RowType tname fields) = do
    values <- CM.zipWithM (encodeEnumField cx g) fields [1..]
    return $ P3.EnumDefinition {
      P3.enumDefinitionName = encodeTypeName tname,
      P3.enumDefinitionValues = unspecifiedField:values,
      P3.enumDefinitionOptions = options}
  where
    unspecifiedField = P3.EnumValue {
      P3.enumValueName = encodeEnumValueName tname $ Name "unspecified",
      P3.enumValueNumber = 0,
      P3.enumValueOptions = []}
    encodeEnumField cx0 g0 (FieldType fname ftype) idx = do
      opts <- findOptions cx0 g0 ftype
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

-- Returns the field and updated context (for counter threading)
encodeFieldType :: Context -> Graph -> Namespace -> FieldType -> Result (P3.Field, Context)
encodeFieldType cx g localNs (FieldType fname ftype) = do
    options <- findOptions cx g ftype
    ft <- encodeType cx g localNs ftype
    let (idx, cx1) = nextCount key_proto_field_index cx
    preserve <- readBooleanAnnotation cx g key_preserveFieldName ftype
    return (P3.Field {
      P3.fieldName = encodeFieldName preserve fname,
      P3.fieldJsonName = Nothing,
      P3.fieldType = ft,
      P3.fieldNumber = idx,
      P3.fieldOptions = options}, cx1)
  where
    encodeType cx0 g0 ns0 typ = case simplifyType typ of
      TypeEither (EitherType lt rt) -> do
        -- Reference the generated helper message type for this Either instantiation
        let ref = StructuralTypeRefEither lt rt
        return $ P3.FieldTypeSimple $ P3.SimpleTypeReference $ structuralTypeName ns0 ref
      TypePair (PairType ft st) -> do
        -- Reference the generated helper message type for this Pair instantiation
        let ref = StructuralTypeRefPair ft st
        return $ P3.FieldTypeSimple $ P3.SimpleTypeReference $ structuralTypeName ns0 ref
      TypeList lt -> do
        P3.FieldTypeRepeated <$> encodeSimpleType cx0 g0 ns0 True lt
      TypeSet st -> do
        -- Encode Set as a repeated field (same as List)
        P3.FieldTypeRepeated <$> encodeSimpleType cx0 g0 ns0 True st
      TypeMap (MapType kt vt) -> P3.FieldTypeMap <$> (P3.MapType <$> encodeSimpleType cx0 g0 ns0 False kt <*> encodeSimpleType cx0 g0 ns0 True vt)
      TypeMaybe ot -> case deannotateType ot of
        TypeLiteral lt -> P3.FieldTypeSimple <$> encodeScalarTypeWrapped cx0 lt
        _ -> encodeType cx0 g0 ns0 ot -- TODO
      TypeUnion (RowType _ fields) -> do
        (pfields, _cx1) <- mapAccumResult (\cx' ft -> encodeFieldType cx' g0 ns0 ft) cx0 fields
        return $ P3.FieldTypeOneof pfields
      _ -> do
        P3.FieldTypeSimple <$> encodeSimpleType cx0 g0 ns0 True typ
    encodeSimpleType cx0 g0 ns0 noms typ = case simplifyType typ of
        TypeLiteral lt -> P3.SimpleTypeScalar <$> encodeScalarType cx0 lt
        TypeRecord (RowType name _) -> forNominal name
        TypeUnion (RowType name _) -> forNominal name
        TypeUnit -> pure $ P3.SimpleTypeReference $ P3.TypeName $ "google.protobuf.Empty"
        TypeVariable name -> if noms
          then forNominal name
          else do
            term <- bindingTerm <$> requireElement cx0 g0 name
            typ <- case DecodeCore.type_ g0 term of
              Left e -> err cx0 (show e)
              Right t -> Right t
            encodeSimpleType cx0 g0 ns0 noms typ
        t -> unexpectedE cx0 "simple type" $ show $ removeTypeAnnotations t
      where
        forNominal name = pure $ P3.SimpleTypeReference $ encodeTypeReference ns0 name

-- Returns the message definition; counter is threaded via context
encodeRecordType :: Context -> Graph -> Namespace -> [P3.Option] -> RowType -> Result P3.MessageDefinition
encodeRecordType cx g localNs options (RowType tname fields) = do
    (pfields, _cx') <- mapAccumResult (\cx' ft -> encodeFieldType cx' g localNs ft) cx fields
    return P3.MessageDefinition {
      P3.messageDefinitionName = encodeTypeName tname,
      P3.messageDefinitionFields = pfields,
      P3.messageDefinitionOptions = options}

encodeScalarType :: Context -> LiteralType -> Result P3.ScalarType
encodeScalarType cx lt = case lt of
  LiteralTypeBinary -> return P3.ScalarTypeBytes
  LiteralTypeBoolean -> return P3.ScalarTypeBool
  LiteralTypeFloat ft -> case ft of
    FloatTypeFloat32 -> return P3.ScalarTypeFloat
    FloatTypeFloat64 -> return P3.ScalarTypeDouble
    _ -> unexpectedE cx "32-bit or 64-bit floating-point type" $ show ft
  LiteralTypeInteger it -> case it of
    IntegerTypeInt32 -> return P3.ScalarTypeInt32
    IntegerTypeInt64 -> return P3.ScalarTypeInt64
    IntegerTypeUint32 -> return P3.ScalarTypeUint32
    IntegerTypeUint64 -> return P3.ScalarTypeUint64
    _ -> unexpectedE cx "32-bit or 64-bit integer type" $ show it
  LiteralTypeString -> return P3.ScalarTypeString

encodeScalarTypeWrapped :: Context -> LiteralType -> Result P3.SimpleType
encodeScalarTypeWrapped cx lt = toType <$> case lt of
    LiteralTypeBinary -> return "Bytes"
    LiteralTypeBoolean -> return "Bool"
    LiteralTypeFloat ft -> case ft of
      FloatTypeFloat32 -> return "Float"
      FloatTypeFloat64 -> return "Double"
      _ -> unexpectedE cx "32-bit or 64-bit floating-point type" $ show ft
    LiteralTypeInteger it -> case it of
      IntegerTypeInt32 -> return "Int32"
      IntegerTypeInt64 -> return "Int64"
      IntegerTypeUint32 -> return "UInt32"
      IntegerTypeUint64 -> return "UInt64"
      _ -> unexpectedE cx "32-bit or 64-bit integer type" $ show it
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

findOptions :: Context -> Graph -> Type -> Result [P3.Option]
findOptions cx g typ = do
  mdesc <- getTypeDescription cx g typ
  bdep <- readBooleanAnnotation cx g key_deprecated typ
  let mdescAnn = fmap (\desc -> P3.Option descriptionOptionName $ P3.ValueString desc) mdesc
  let mdepAnn = if bdep then Just (P3.Option deprecatedOptionName $ P3.ValueBoolean True) else Nothing
  return $ Y.catMaybes [mdescAnn, mdepAnn]

isEnumFields :: [FieldType] -> Bool
isEnumFields fields = L.foldl (&&) True $ fmap isEnumField fields
  where
    isEnumField = Schemas.isUnitType . simplifyType . fieldTypeType

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

readBooleanAnnotation :: Context -> Graph -> Name -> Type -> Result Bool
readBooleanAnnotation cx g key typ = case M.lookup key (typeAnnotationInternal typ) of
  Nothing -> return False
  Just term -> ExtractCore.boolean cx g term

-- Note: this should probably be done in the term adapters
simplifyType :: Type -> Type
simplifyType typ = case deannotateType typ of
  TypeWrap (WrappedType _ t) -> simplifyType t
  t -> t
