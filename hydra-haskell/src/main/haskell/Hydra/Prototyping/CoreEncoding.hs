module Hydra.Prototyping.CoreEncoding (
    encodeApplication,
    encodeAtomicType,
    encodeAtomicValue,
    encodeAtomicVariant,
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
import Hydra.Impl.Haskell.Dsl

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


encodeApplication :: (Default a, Ord a) => Application a -> Term a
encodeApplication (Application f a) = record [
  Field _Application_function $ encodeTerm f,
  Field _Application_argument $ encodeTerm f]

encodeAtomicType :: Default a => AtomicType -> Term a
encodeAtomicType at = case at of
  AtomicTypeBinary -> unitVariant _AtomicType_binary
  AtomicTypeBoolean -> unitVariant _AtomicType_boolean
  AtomicTypeFloat ft -> variant _AtomicType_float $ encodeFloatType ft
  AtomicTypeInteger it -> variant _AtomicType_integer $ encodeIntegerType it
  AtomicTypeString -> unitVariant _AtomicType_string

encodeAtomicValue :: Default a => AtomicValue -> Term a
encodeAtomicValue = atomic

encodeAtomicVariant :: Default a => AtomicVariant -> Term a
encodeAtomicVariant av = unitVariant $ case av of
  AtomicVariantBinary -> _AtomicVariant_binary
  AtomicVariantBoolean -> _AtomicVariant_boolean
  AtomicVariantFloat -> _AtomicVariant_float
  AtomicVariantInteger -> _AtomicVariant_integer
  AtomicVariantString -> _AtomicVariant_string

encodeField :: (Default a, Ord a) => Field a -> Term a
encodeField (Field name term) = record [
  Field _Field_name $ stringValue name,
  Field _Field_term $ encodeTerm term]

encodeFieldType :: Default a => FieldType -> Term a
encodeFieldType (FieldType fname t) = record [
  Field _FieldType_name $ stringTerm fname,
  Field _FieldType_type $ encodeType t]

encodeFloatType :: Default a => FloatType -> Term a
encodeFloatType ft = unitVariant $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

encodeFloatVariant :: Default a => FloatVariant -> Term a
encodeFloatVariant fv = unitVariant $ case fv of
  FloatVariantBigfloat -> _FloatVariant_bigfloat
  FloatVariantFloat32 -> _FloatVariant_float32
  FloatVariantFloat64 -> _FloatVariant_float64

encodeFunction :: (Default a, Ord a) => Function a -> Term a
encodeFunction f = case f of
  FunctionCases cases -> variant _Function_cases $ list $ encodeField <$> cases
  FunctionCompareTo other -> variant _Function_compareTo $ encodeTerm other
  FunctionData -> unitVariant _Function_data
  FunctionLambda l -> variant _Function_lambda $ encodeLambda l
  FunctionPrimitive name -> variant _Function_primitive $ stringValue name
  FunctionProjection fname -> variant _Function_projection $ stringValue fname

encodeFunctionType :: Default a => FunctionType -> Term a
encodeFunctionType (FunctionType dom cod) = record [
  Field _FunctionType_domain $ encodeType dom,
  Field _FunctionType_codomain $ encodeType cod]

encodeIntegerType :: Default a => IntegerType -> Term a
encodeIntegerType it = unitVariant $ case it of
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
encodeIntegerVariant iv = unitVariant $ case iv of
  IntegerVariantBigint -> _IntegerType_bigint
  IntegerVariantInt8 -> _IntegerType_int8
  IntegerVariantInt16 -> _IntegerType_int16
  IntegerVariantInt32 -> _IntegerType_int32
  IntegerVariantInt64 -> _IntegerType_int64
  IntegerVariantUint8 -> _IntegerType_uint8
  IntegerVariantUint16 -> _IntegerType_uint16
  IntegerVariantUint32 -> _IntegerType_uint32
  IntegerVariantUint64 -> _IntegerType_uint64

encodeLambda :: (Default a, Ord a) => Lambda a -> Term a
encodeLambda (Lambda v b) = record [
  Field _Lambda_parameter $ stringValue v,
  Field _Lambda_body $ encodeTerm b]

encodeMapType :: Default a => MapType -> Term a
encodeMapType (MapType kt vt) = record [
  Field _MapType_keys $ encodeType kt,
  Field _MapType_values $ encodeType vt]

encodeTerm :: (Default a, Ord a) => Term a -> Term a
encodeTerm term = case termData term of
  ExpressionApplication a -> variant _Expression_application $ encodeApplication a
  ExpressionAtomic av -> variant _Expression_atomic $ encodeAtomicValue av
  ExpressionElement name -> variant _Expression_element $ stringValue name
  ExpressionFunction f -> variant _Expression_function $ encodeFunction f
  ExpressionList terms -> variant _Expression_list $ list $ encodeTerm <$> terms
  ExpressionMap m -> variant _Expression_map $ map $ M.fromList $ encodePair <$> M.toList m
    where encodePair (k, v) = (encodeTerm k, encodeTerm v)
  ExpressionOptional m -> variant _Expression_optional $ optional $ encodeTerm <$> m
  ExpressionRecord fields -> variant _Expression_record $ list $ encodeField <$> fields
  ExpressionSet terms -> variant _Expression_set $ set $ S.fromList $ encodeTerm <$> S.toList terms
  ExpressionUnion field -> variant _Expression_union $ encodeField field
  ExpressionVariable var -> variant _Expression_variable $ stringValue var

encodeType :: Default a => Type -> Term a
encodeType typ = case typ of
  TypeAtomic at -> variant _Type_atomic $ encodeAtomicType at
  TypeElement t -> variant _Type_element $ encodeType t
  TypeFunction ft -> variant _Type_function $ encodeFunctionType ft
  TypeList t -> variant _Type_list $ encodeType t
  TypeMap mt -> variant _Type_map $ encodeMapType mt
  TypeNominal name -> variant _Type_nominal $ stringTerm name
  TypeOptional t -> variant _Type_optional $ encodeType t
  TypeRecord fields -> variant _Type_record $ list $ fmap encodeFieldType fields
  TypeSet t -> variant _Type_set $ encodeType t
  TypeUnion fields -> variant _Type_union $ list $ fmap encodeFieldType fields
  TypeUniversal ut -> variant _Type_universal $ encodeUniversalType ut
  TypeVariable var -> variant _Type_variable $ stringTerm var

encodeTypeVariant :: Default a => TypeVariant -> Term a
encodeTypeVariant tv = unitVariant $ case tv of
  TypeVariantAtomic -> _TypeVariant_atomic
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
encodeUniversalType (UniversalType var body) = record [
  Field _UniversalType_variable $ stringValue var,
  Field _UniversalType_body $ encodeType body]
