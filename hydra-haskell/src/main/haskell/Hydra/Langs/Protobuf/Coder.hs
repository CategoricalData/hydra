module Hydra.Langs.Protobuf.Coder (moduleToProtobuf) where

import Hydra.Kernel
import Hydra.Langs.Protobuf.Language
import qualified Hydra.Langs.Protobuf.Proto3 as P3
import qualified Hydra.Lib.Strings as Strings
import Hydra.Langs.Protobuf.Language
import Hydra.Langs.Protobuf.Serde
import Hydra.Tools.Serialization

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


moduleToProtobuf :: (Ord a, Read a, Show a) => Module a -> Flow (Graph a) (M.Map FilePath String)
moduleToProtobuf mod = do
    files <- transformModule protobufLanguage encodeTerm constructModule mod
    return $ M.fromList (mapPair <$> M.toList files)
  where
    mapPair (path, sf) = (path, printExpr $ parenthesize $ writeProtoFile sf)
    encodeTerm _ = fail "term-level encoding it not yet supported"

--

checkIsStringType :: Show a => Type a -> Flow (Graph a) ()
checkIsStringType typ = case stripType typ of
  TypeLiteral lt -> case lt of
    LiteralTypeString -> pure ()
    _ -> unexpected "string type" $ show lt
  TypeVariable name -> requireType name >>= checkIsStringType
  TypeWrap (Nominal _ t) -> checkIsStringType t
  _ -> unexpected "literal (string) type" $ show typ

constructModule :: (Ord a, Read a, Show a)
  => Module a
  -> M.Map (Type a) (Coder (Graph a) (Graph a) (Term a) ())
  -> [(Element a, TypedTerm a)]
  -> Flow (Graph a) (M.Map FilePath P3.ProtoFile)
constructModule mod@(Module ns els _ desc) _ pairs = do
    imports <- (fmap namespaceToFileReference . S.toList) <$> moduleDependencyNamespaces True False True False mod
    definitions <- CM.mapM toDef pairs
    let pfile = P3.ProtoFile {
      P3.protoFilePackage = namespaceToPackageName ns,
      P3.protoFileImports = imports,
      P3.protoFileTypes = definitions,
      P3.protoFileOptions = []}
    return $ M.singleton path pfile
  where
    path = P3.unFileReference $ namespaceToFileReference ns
    toDef (el, (TypedTerm typ term)) = do
      if isType typ
        then coreDecodeType term >>= encodeDefinition ns (elementName el)
        else fail $ "mapping of non-type elements to PDL is not yet supported: " ++ unName (elementName el)

encodeDefinition :: (Eq a, Ord a, Show a) => Namespace -> Name -> Type a -> Flow (Graph a) P3.Definition
encodeDefinition localNs name typ = do
    resetCount "proto_field_index"
    case stripType typ of
      TypeRecord rt -> P3.DefinitionMessage <$> encodeRecordType localNs rt
      TypeUnion rt -> if isEnumDefinition typ
        then pure $ P3.DefinitionEnum $ encodeEnumDefinition rt
        else fail $ "union type is not an enumeration: " ++ show typ
      t -> encodeDefinition localNs name $ wrapAsRecordType t
  where
    wrapAsRecordType t = TypeRecord $ RowType name Nothing [FieldType (FieldName "value") t]

encodeEnumDefinition :: RowType a -> P3.EnumDefinition
encodeEnumDefinition (RowType tname _ fields) = P3.EnumDefinition {
    P3.enumDefinitionName = encodeTypeName tname,
    P3.enumDefinitionValues = unknownField:(L.zipWith encodeEnumField fields [1..]),
    P3.enumDefinitionOptions = []}
  where
    unknownField = P3.EnumValue {
      P3.enumValueName = P3.EnumValueName "UNKNOWN",
      P3.enumValueNumber = 0,
      P3.enumValueOptions = []}
    encodeEnumField (FieldType fname _) idx = P3.EnumValue {
      P3.enumValueName = encodeEnumValueName fname,
      P3.enumValueNumber = idx,
      P3.enumValueOptions = []}

encodeEnumValueName :: FieldName -> P3.EnumValueName
encodeEnumValueName = P3.EnumValueName . Strings.toUpper . unFieldName

encodeFieldName :: FieldName -> P3.FieldName
encodeFieldName = P3.FieldName . unFieldName

encodeFieldType :: (Ord a, Show a) => Namespace -> FieldType a -> Flow (Graph a) P3.Field
encodeFieldType localNs (FieldType fname ftype) = do
    idx <- nextCount "proto_field_index"
    ft <- encodeType ftype
    return $ P3.Field {
      P3.fieldName = encodeFieldName fname,
      P3.fieldJsonName = Nothing,
      P3.fieldType = ft,
      P3.fieldNumber = idx,
      P3.fieldOptions = []}
  where
    encodeType typ = case stripType typ of
      TypeList lt -> P3.FieldTypeRepeated <$> encodeSimpleType lt
      TypeMap (MapType kt vt) -> do
        checkIsStringType kt
        P3.FieldTypeMap <$> encodeSimpleType vt
      TypeOptional ot -> encodeType ot -- TODO
      TypeUnion (RowType _ _ fields) -> do
        pfields <- CM.mapM (encodeFieldType localNs) fields
        return $ P3.FieldTypeOneof pfields
      TypeWrap (Nominal _ typ1) -> encodeType typ1
      _ -> P3.FieldTypeSimple <$> encodeSimpleType typ
    encodeSimpleType typ = case stripType typ of
      TypeLiteral lt -> P3.SimpleTypeScalar <$> encodeScalarType lt
      TypeVariable name -> pure $ P3.SimpleTypeReference $ encodeTypeReference localNs name
      TypeWrap (Nominal _ typ1) -> encodeSimpleType typ1
      t -> unexpected "simple type" $ show $ removeTypeAnnotations t

encodeRecordType :: (Ord a, Show a) => Namespace -> RowType a -> Flow (Graph a) P3.MessageDefinition
encodeRecordType localNs (RowType tname _ fields) = do
    pfields <- CM.mapM (encodeFieldType localNs) fields
    return P3.MessageDefinition {
      P3.messageDefinitionName = encodeTypeName tname,
      P3.messageDefinitionFields = pfields,
      P3.messageDefinitionOptions = []}

encodeScalarType :: LiteralType -> Flow s P3.ScalarType
encodeScalarType lt = case lt of
  LiteralTypeBinary -> return P3.ScalarTypeBytes
  LiteralTypeFloat ft -> case ft of
    FloatTypeFloat32 -> return P3.ScalarTypeFloat
    FloatTypeFloat64 -> return P3.ScalarTypeDouble
    _ -> unexpected "32-bit or 64-bit floating-poind type" $ show ft
  LiteralTypeInteger it -> case it of
    IntegerTypeInt32 -> return P3.ScalarTypeInt32
    IntegerTypeInt64 -> return P3.ScalarTypeInt64
    IntegerTypeUint32 -> return P3.ScalarTypeUint32
    IntegerTypeUint64 -> return P3.ScalarTypeUint64
    _ -> unexpected "32-bit or 64-bit integer type" $ show it
  LiteralTypeString -> return P3.ScalarTypeString
  _ -> unexpected "binary, float, integer, or string type" $ show lt

encodeTypeName :: Name -> P3.TypeName
encodeTypeName = P3.TypeName . localNameOfEager

encodeTypeReference :: Namespace -> Name -> P3.TypeName
encodeTypeReference localNs name = P3.TypeName $ if ns == Just localNs
    then local
    else unName name -- TODO
  where
    QualifiedName ns local = qualifyNameEager name

isEnumFields :: Eq a => [FieldType a] -> Bool
isEnumFields fields = L.foldl (&&) True $ fmap isEnumField fields
  where
    isEnumField = isUnitType . stripType . fieldTypeType

isEnumDefinition :: Eq a => Type a -> Bool
isEnumDefinition typ = case stripType typ of
  TypeUnion (RowType _ _ fields) -> isEnumFields fields
  _ -> False

isEnumDefinitionReference :: (Eq a, Show a) => Name -> Flow (Graph a) Bool
isEnumDefinitionReference name = isEnumDefinition <$> ((elementData <$> requireElement name) >>= coreDecodeType)

namespaceToFileReference :: Namespace -> P3.FileReference
namespaceToFileReference (Namespace ns) = P3.FileReference $ ns ++ ".proto"

namespaceToPackageName :: Namespace -> P3.PackageName
namespaceToPackageName (Namespace ns) = P3.PackageName $ Strings.intercalate "." $ Strings.splitOn "/" ns
