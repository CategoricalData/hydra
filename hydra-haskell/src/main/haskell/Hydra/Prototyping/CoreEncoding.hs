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

import Hydra.V2.Core
import Hydra.Impl.Haskell.Dsl

import qualified Data.Map as M
import qualified Data.Set as S


encodeApplication :: Application -> Term
encodeApplication (Application f a) = ExpressionRecord [
  Field _Application_function $ encodeTerm f,
  Field _Application_argument $ encodeTerm f]

encodeAtomicType :: AtomicType -> Term
encodeAtomicType at = case at of
  AtomicTypeBinary -> unitVariant _AtomicType_binary
  AtomicTypeBoolean -> unitVariant _AtomicType_boolean
  AtomicTypeFloat ft -> variant _AtomicType_float $ encodeFloatType ft
  AtomicTypeInteger it -> variant _AtomicType_integer $ encodeIntegerType it
  AtomicTypeString -> unitVariant _AtomicType_string

encodeAtomicValue :: AtomicValue -> Term
encodeAtomicValue = ExpressionAtomic

encodeAtomicVariant :: AtomicVariant -> Term
encodeAtomicVariant av = unitVariant $ case av of
  AtomicVariantBinary -> _AtomicVariant_binary
  AtomicVariantBoolean -> _AtomicVariant_boolean
  AtomicVariantFloat -> _AtomicVariant_float
  AtomicVariantInteger -> _AtomicVariant_integer
  AtomicVariantString -> _AtomicVariant_string

encodeField :: Field -> Term
encodeField (Field name term) = ExpressionRecord [
  Field _Field_name $ stringValue name,
  Field _Field_term $ encodeTerm term]

encodeFieldType :: FieldType -> Term
encodeFieldType (FieldType fname t) = ExpressionRecord [
  Field _FieldType_name $ stringTerm fname,
  Field _FieldType_type $ encodeType t]

encodeFloatType :: FloatType -> Term
encodeFloatType ft = unitVariant $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

encodeFloatVariant :: FloatVariant -> Term
encodeFloatVariant fv = unitVariant $ case fv of
  FloatVariantBigfloat -> _FloatVariant_bigfloat
  FloatVariantFloat32 -> _FloatVariant_float32
  FloatVariantFloat64 -> _FloatVariant_float64

encodeFunction :: Function -> Term
encodeFunction f = case f of
  FunctionCases cases -> variant _Function_cases $ ExpressionList $ encodeField <$> cases
  FunctionCompareTo other -> variant _Function_compareTo $ encodeTerm other
  FunctionData -> unitVariant _Function_data
  FunctionLambda l -> variant _Function_lambda $ encodeLambda l
  FunctionPrimitive name -> variant _Function_primitive $ stringValue name
  FunctionProjection fname -> variant _Function_projection $ stringValue fname

encodeFunctionType :: FunctionType -> Term
encodeFunctionType (FunctionType dom cod) = ExpressionRecord [
  Field _FunctionType_domain $ encodeType dom,
  Field _FunctionType_codomain $ encodeType cod]

encodeIntegerType :: IntegerType -> Term
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

encodeIntegerVariant :: IntegerVariant -> Term
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

encodeLambda :: Lambda -> Term
encodeLambda (Lambda v b) = ExpressionRecord [
  Field _Lambda_parameter $ stringValue v,
  Field _Lambda_body $ encodeTerm b]

encodeMapType :: MapType -> Term
encodeMapType (MapType kt vt) = ExpressionRecord [
  Field _MapType_keys $ encodeType kt,
  Field _MapType_values $ encodeType vt]

encodeTerm :: Term -> Term
encodeTerm term = case term of
  ExpressionApplication a -> variant _Term_application $ encodeApplication a
  ExpressionAtomic av -> variant _Term_atomic $ encodeAtomicValue av
  ExpressionElement name -> variant _Term_element $ stringValue name
  ExpressionFunction f -> variant _Term_function $ encodeFunction f
  ExpressionList terms -> variant _Term_list $ ExpressionList $ encodeTerm <$> terms
  ExpressionMap map -> variant _Term_map $ ExpressionMap $ M.fromList $ encodePair <$> M.toList map
    where encodePair (k, v) = (encodeTerm k, encodeTerm v)
  ExpressionOptional m -> variant _Term_optional $ ExpressionOptional $ encodeTerm <$> m
  ExpressionRecord fields -> variant _Term_record $ ExpressionList $ encodeField <$> fields
  ExpressionSet terms -> variant _Term_set $ ExpressionSet $ S.fromList $ encodeTerm <$> S.toList terms
  ExpressionUnion field -> variant _Term_union $ encodeField field
  ExpressionVariable var -> variant _Term_variable $ stringValue var

encodeType :: Type -> Term
encodeType typ = case typ of
  TypeAtomic at -> variant _Type_atomic $ encodeAtomicType at
  TypeElement t -> variant _Type_element $ encodeType t
  TypeFunction ft -> variant _Type_function $ encodeFunctionType ft
  TypeList t -> variant _Type_list $ encodeType t
  TypeMap mt -> variant _Type_map $ encodeMapType mt
  TypeNominal name -> variant _Type_nominal $ stringTerm name
  TypeOptional t -> variant _Type_optional $ encodeType t
  TypeRecord fields -> variant _Type_record $ ExpressionList $ fmap encodeFieldType fields
  TypeSet t -> variant _Type_set $ encodeType t
  TypeUnion fields -> variant _Type_union $ ExpressionList $ fmap encodeFieldType fields
  TypeAbstract ut -> variant _Type_abstract $ encodeAbstractType ut
  TypeVariable var -> variant _Type_variable $ stringTerm var

encodeTypeVariant :: TypeVariant -> Term
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
  TypeVariantAbstract -> _TypeVariant_abstract
  TypeVariantVariable -> _TypeVariant_variable

encodeAbstractType :: AbstractType -> Term
encodeAbstractType (AbstractType var body) = ExpressionRecord [
  Field _AbstractType_variable $ stringValue var,
  Field _AbstractType_body $ encodeType body]
