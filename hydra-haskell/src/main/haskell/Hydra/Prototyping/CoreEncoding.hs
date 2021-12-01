module Hydra.Prototyping.CoreEncoding (
    encodeApplication,
    encodeLiteralType,
    encodeLiteral,
    encodeLiteralVariant,
    encodeField,
    encodeFieldType,
    encodeFloatType,
    encodeFloatVariant,
    encodeFunction,
    encodeFunctionType,
    encodeIntegerType,
    encodeIntegerVariant,
    encodeLambda,
    encodeProjection,
    encodeTerm,
    encodeType,
    encodeTypeVariant,
  ) where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Terms

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


encodeApplication :: (Default a, Ord a) => Application a -> Term a
encodeApplication (Application f a) = nominal _Application $ record [
  Field _Application_function $ encodeTerm f,
  Field _Application_argument $ encodeTerm f]

encodeLiteralType :: Default a => LiteralType -> Term a
encodeLiteralType at = case at of
  LiteralTypeBinary -> unitVariant _LiteralType _LiteralType_binary
  LiteralTypeBoolean -> unitVariant _LiteralType _LiteralType_boolean
  LiteralTypeFloat ft -> variant _LiteralType _LiteralType_float $ encodeFloatType ft
  LiteralTypeInteger it -> variant _LiteralType _LiteralType_integer $ encodeIntegerType it
  LiteralTypeString -> unitVariant _LiteralType _LiteralType_string

encodeLiteral :: Default a => Literal -> Term a
encodeLiteral = atomic

encodeLiteralVariant :: Default a => LiteralVariant -> Term a
encodeLiteralVariant av = unitVariant _LiteralVariant $ case av of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

encodeField :: (Default a, Ord a) => Field a -> Term a
encodeField (Field name term) = nominal _Field $ record [
  Field _Field_name $ stringValue name,
  Field _Field_term $ encodeTerm term]

encodeFieldType :: Default a => FieldType -> Term a
encodeFieldType (FieldType fname t) = nominal _FieldType $ record [
  Field _FieldType_name $ stringTerm fname,
  Field _FieldType_type $ encodeType t]

encodeFloatType :: Default a => FloatType -> Term a
encodeFloatType ft = unitVariant _FloatType $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

encodeFloatVariant :: Default a => FloatVariant -> Term a
encodeFloatVariant fv = unitVariant _FloatVariant $ case fv of
  FloatVariantBigfloat -> _FloatVariant_bigfloat
  FloatVariantFloat32 -> _FloatVariant_float32
  FloatVariantFloat64 -> _FloatVariant_float64

encodeFunction :: (Default a, Ord a) => Function a -> Term a
encodeFunction f = case f of
  FunctionCases cases -> variant _Function _Function_cases $ list $ encodeField <$> cases
  FunctionCompareTo other -> variant _Function _Function_compareTo $ encodeTerm other
  FunctionData -> unitVariant _Function _Function_data
  FunctionLambda l -> variant _Function _Function_lambda $ encodeLambda l
  FunctionPrimitive name -> variant _Function _Function_primitive $ stringValue name
  FunctionProjection prj -> variant _Function _Function_projection $ encodeProjection prj

encodeFunctionType :: Default a => FunctionType -> Term a
encodeFunctionType (FunctionType dom cod) = nominal _FunctionType $ record [
  Field _FunctionType_domain $ encodeType dom,
  Field _FunctionType_codomain $ encodeType cod]

encodeIntegerType :: Default a => IntegerType -> Term a
encodeIntegerType it = unitVariant _IntegerType $ case it of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

encodeIntegerVariant :: Default a => IntegerVariant -> Term a
encodeIntegerVariant iv = unitVariant _IntegerVariant $ case iv of
  IntegerVariantBigint -> _IntegerVariant_bigint
  IntegerVariantInt8 -> _IntegerVariant_int8
  IntegerVariantInt16 -> _IntegerVariant_int16
  IntegerVariantInt32 -> _IntegerVariant_int32
  IntegerVariantInt64 -> _IntegerVariant_int64
  IntegerVariantUint8 -> _IntegerVariant_uint8
  IntegerVariantUint16 -> _IntegerVariant_uint16
  IntegerVariantUint32 -> _IntegerVariant_uint32
  IntegerVariantUint64 -> _IntegerVariant_uint64

encodeLambda :: (Default a, Ord a) => Lambda a -> Term a
encodeLambda (Lambda v b) = nominal _Lambda $ record [
  Field _Lambda_parameter $ stringValue v,
  Field _Lambda_body $ encodeTerm b]

encodeMapType :: Default a => MapType -> Term a
encodeMapType (MapType kt vt) = nominal _MapType $ record [
  Field _MapType_keys $ encodeType kt,
  Field _MapType_values $ encodeType vt]

encodeNominalTerm :: (Default a, Ord a) => NominalTerm a -> Term a
encodeNominalTerm (NominalTerm name term) = nominal _NominalTerm $ record [
  Field _NominalTerm_typeName $ stringValue name,
  Field _NominalTerm_term $ encodeTerm term]

encodeProjection :: Default a => Projection -> Term a
encodeProjection (Projection fname rname) = nominal _Projection $ record [
  Field _Projection_field $ stringValue fname,
  Field _Projection_context $ stringValue rname]

encodeTerm :: (Default a, Ord a) => Term a -> Term a
encodeTerm term = case termData term of
  ExpressionApplication a -> variant _Expression _Expression_application $ encodeApplication a
  ExpressionLiteral av -> variant _Expression _Expression_literal $ encodeLiteral av
  ExpressionElement name -> variant _Expression _Expression_element $ stringValue name
  ExpressionFunction f -> variant _Expression _Expression_function $ encodeFunction f
  ExpressionList terms -> variant _Expression _Expression_list $ list $ encodeTerm <$> terms
  ExpressionMap m -> variant _Expression _Expression_map $ map $ M.fromList $ encodePair <$> M.toList m
    where encodePair (k, v) = (encodeTerm k, encodeTerm v)
  ExpressionNominal ntt -> variant _Expression _Expression_nominal $ encodeNominalTerm ntt
  ExpressionOptional m -> variant _Expression _Expression_optional $ optional $ encodeTerm <$> m
  ExpressionRecord fields -> variant _Expression _Expression_record $ list $ encodeField <$> fields
  ExpressionSet terms -> variant _Expression _Expression_set $ set $ S.fromList $ encodeTerm <$> S.toList terms
  ExpressionUnion vr -> variant _Expression _Expression_union $ encodeUnionExpression vr
  ExpressionVariable var -> variant _Expression _Expression_variable $ stringValue var

encodeType :: Default a => Type -> Term a
encodeType typ = nominal _Type $ case typ of
  TypeLiteral at -> variant _Type _Type_literal $ encodeLiteralType at
  TypeElement t -> variant _Type _Type_element $ encodeType t
  TypeFunction ft -> variant _Type _Type_function $ encodeFunctionType ft
  TypeList t -> variant _Type _Type_list $ encodeType t
  TypeMap mt -> variant _Type _Type_map $ encodeMapType mt
  TypeNominal name -> variant _Type _Type_nominal $ stringTerm name
  TypeOptional t -> variant _Type _Type_optional $ encodeType t
  TypeRecord fields -> variant _Type _Type_record $ list $ fmap encodeFieldType fields
  TypeSet t -> variant _Type _Type_set $ encodeType t
  TypeUnion fields -> variant _Type _Type_union $ list $ fmap encodeFieldType fields
  TypeUniversal ut -> variant _Type _Type_universal $ encodeUniversalType ut
  TypeVariable var -> variant _Type _Type_variable $ stringTerm var

encodeTypeVariant :: Default a => TypeVariant -> Term a
encodeTypeVariant tv = unitVariant _TypeVariant $ case tv of
  TypeVariantLiteral -> _TypeVariant_literal
  TypeVariantElement -> _TypeVariant_element
  TypeVariantFunction -> _TypeVariant_function
  TypeVariantList -> _TypeVariant_list
  TypeVariantMap -> _TypeVariant_map
  TypeVariantNominal -> _TypeVariant_nominal
  TypeVariantOptional -> _TypeVariant_optional
  TypeVariantRecord -> _TypeVariant_record
  TypeVariantSet -> _TypeVariant_set
  TypeVariantUnion -> _TypeVariant_union
  TypeVariantUniversal -> _TypeVariant_universal
  TypeVariantVariable -> _TypeVariant_variable

encodeUniversalType :: Default a => UniversalType -> Term a
encodeUniversalType (UniversalType var body) = nominal _UniversalType $ record [
  Field _UniversalType_variable $ stringValue var,
  Field _UniversalType_body $ encodeType body]

encodeUnionExpression :: (Default a, Ord a) => UnionExpression a -> Term a
encodeUnionExpression (UnionExpression context field) = nominal _UnionExpression $ record [
  Field _UnionExpression_field $ encodeField field,
  Field _UnionExpression_context $ stringValue context]
