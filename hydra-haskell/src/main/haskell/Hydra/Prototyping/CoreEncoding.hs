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
    encodeTerm,
    encodeType,
    encodeTypeVariant,
  ) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Extras

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


encodeApplication :: (Default a, Ord a) => Context a -> Application a -> Term a
encodeApplication cx (Application f a) = nominalRecord cx _Application [
  Field _Application_function $ encodeTerm cx f,
  Field _Application_argument $ encodeTerm cx f]

encodeLiteralType :: Default a => Context a -> LiteralType -> Term a
encodeLiteralType cx at = case at of
  LiteralTypeBinary -> unitVariant _LiteralType _LiteralType_binary
  LiteralTypeBoolean -> unitVariant _LiteralType _LiteralType_boolean
  LiteralTypeFloat ft -> variant _LiteralType _LiteralType_float $ encodeFloatType cx ft
  LiteralTypeInteger it -> variant _LiteralType _LiteralType_integer $ encodeIntegerType cx it
  LiteralTypeString -> unitVariant _LiteralType _LiteralType_string

encodeLiteral :: Default a => Context a -> Literal -> Term a
encodeLiteral cx = atomic

encodeLiteralVariant :: Default a => Context a -> LiteralVariant -> Term a
encodeLiteralVariant cx av = unitVariant _LiteralVariant $ case av of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

encodeField :: (Default a, Ord a) => Context a -> Field a -> Term a
encodeField cx (Field name term) = nominalRecord cx _Field [
  Field _Field_name $ stringValue name,
  Field _Field_term $ encodeTerm cx term]

encodeFieldType :: Default a => Context a -> FieldType -> Term a
encodeFieldType cx (FieldType fname t) = nominalRecord cx _FieldType [
  Field _FieldType_name $ stringTerm fname,
  Field _FieldType_type $ encodeType cx t]

encodeFloatType :: Default a => Context a -> FloatType -> Term a
encodeFloatType cx ft = unitVariant _FloatType $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

encodeFloatVariant :: Default a => Context a -> FloatVariant -> Term a
encodeFloatVariant cx fv = unitVariant _FloatVariant $ case fv of
  FloatVariantBigfloat -> _FloatVariant_bigfloat
  FloatVariantFloat32 -> _FloatVariant_float32
  FloatVariantFloat64 -> _FloatVariant_float64

encodeFunction :: (Default a, Ord a) => Context a -> Function a -> Term a
encodeFunction cx f = case f of
  FunctionCases cases -> variant _Function _Function_cases $ list $ encodeField cx <$> cases
  FunctionCompareTo other -> variant _Function _Function_compareTo $ encodeTerm cx other
  FunctionData -> unitVariant _Function _Function_data
  FunctionLambda l -> variant _Function _Function_lambda $ encodeLambda cx l
  FunctionPrimitive name -> variant _Function _Function_primitive $ stringValue name
  FunctionProjection fname -> variant _Function _Function_projection $ stringValue fname

encodeFunctionType :: Default a => Context a -> FunctionType -> Term a
encodeFunctionType cx (FunctionType dom cod) = nominalRecord cx _FunctionType [
  Field _FunctionType_domain $ encodeType cx dom,
  Field _FunctionType_codomain $ encodeType cx cod]

encodeIntegerType :: Default a => Context a -> IntegerType -> Term a
encodeIntegerType cx it = unitVariant _IntegerType $ case it of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

encodeIntegerVariant :: Default a => Context a -> IntegerVariant -> Term a
encodeIntegerVariant cx iv = unitVariant _IntegerVariant $ case iv of
  IntegerVariantBigint -> _IntegerVariant_bigint
  IntegerVariantInt8 -> _IntegerVariant_int8
  IntegerVariantInt16 -> _IntegerVariant_int16
  IntegerVariantInt32 -> _IntegerVariant_int32
  IntegerVariantInt64 -> _IntegerVariant_int64
  IntegerVariantUint8 -> _IntegerVariant_uint8
  IntegerVariantUint16 -> _IntegerVariant_uint16
  IntegerVariantUint32 -> _IntegerVariant_uint32
  IntegerVariantUint64 -> _IntegerVariant_uint64

encodeLambda :: (Default a, Ord a) => Context a -> Lambda a -> Term a
encodeLambda cx (Lambda v b) = nominalRecord cx _Lambda [
  Field _Lambda_parameter $ stringValue v,
  Field _Lambda_body $ encodeTerm cx b]

encodeMapType :: Default a => Context a -> MapType -> Term a
encodeMapType cx (MapType kt vt) = nominalRecord cx _MapType [
  Field _MapType_keys $ encodeType cx kt,
  Field _MapType_values $ encodeType cx vt]

encodeNominalTerm :: (Default a, Ord a) => Context a -> NominalTerm a -> Term a
encodeNominalTerm cx (NominalTerm name term) = nominalRecord cx _NominalTerm [
  Field _NominalTerm_typeName $ stringValue name,
  Field _NominalTerm_term $ encodeTerm cx term]

encodeTerm :: (Default a, Ord a) => Context a -> Term a -> Term a
encodeTerm cx term = case termData term of
  ExpressionApplication a -> variant _Expression _Expression_application $ encodeApplication cx a
  ExpressionLiteral av -> variant _Expression _Expression_literal $ encodeLiteral cx av
  ExpressionElement name -> variant _Expression _Expression_element $ stringValue name
  ExpressionFunction f -> variant _Expression _Expression_function $ encodeFunction cx f
  ExpressionList terms -> variant _Expression _Expression_list $ list $ encodeTerm cx <$> terms
  ExpressionMap m -> variant _Expression _Expression_map $ map $ M.fromList $ encodePair <$> M.toList m
    where encodePair (k, v) = (encodeTerm cx k, encodeTerm cx v)
  ExpressionNominal ntt -> variant _Expression _Expression_nominal $ encodeNominalTerm cx ntt
  ExpressionOptional m -> variant _Expression _Expression_optional $ optional $ encodeTerm cx <$> m
  ExpressionRecord fields -> variant _Expression _Expression_record $ list $ encodeField cx <$> fields
  ExpressionSet terms -> variant _Expression _Expression_set $ set $ S.fromList $ encodeTerm cx <$> S.toList terms
  ExpressionUnion vr -> variant _Expression _Expression_union $ encodeUnionExpression cx vr
  ExpressionVariable var -> variant _Expression _Expression_variable $ stringValue var

encodeType :: Default a => Context a -> Type -> Term a
encodeType cx typ = case typ of
  TypeLiteral at -> variant _Type _Type_literal $ encodeLiteralType cx at
  TypeElement t -> variant _Type _Type_element $ encodeType cx t
  TypeFunction ft -> variant _Type _Type_function $ encodeFunctionType cx ft
  TypeList t -> variant _Type _Type_list $ encodeType cx t
  TypeMap mt -> variant _Type _Type_map $ encodeMapType cx mt
  TypeNominal name -> variant _Type _Type_nominal $ stringTerm name
  TypeOptional t -> variant _Type _Type_optional $ encodeType cx t
  TypeRecord fields -> variant _Type _Type_record $ list $ fmap (encodeFieldType cx) fields
  TypeSet t -> variant _Type _Type_set $ encodeType cx t
  TypeUnion fields -> variant _Type _Type_union $ list $ fmap (encodeFieldType cx) fields
  TypeUniversal ut -> variant _Type _Type_universal $ encodeUniversalType cx ut
  TypeVariable var -> variant _Type _Type_variable $ stringTerm var

encodeTypeVariant :: Default a => Context a -> TypeVariant -> Term a
encodeTypeVariant _ tv = unitVariant _TypeVariant $ case tv of
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

encodeUniversalType :: Default a => Context a -> UniversalType -> Term a
encodeUniversalType cx (UniversalType var body) = nominalRecord cx _UniversalType [
  Field _UniversalType_variable $ stringValue var,
  Field _UniversalType_body $ encodeType cx body]

encodeUnionExpression :: (Default a, Ord a) => Context a -> UnionExpression a -> Term a
encodeUnionExpression cx (UnionExpression context field) = nominalRecord cx _UnionExpression [
  Field _UnionExpression_field $ encodeField cx field,
  Field _UnionExpression_context $ stringValue context]
