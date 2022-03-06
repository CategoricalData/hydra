module Hydra.CoreEncoding (
    encodeApplication,
    encodeLiteralType,
    encodeLiteral,
    encodeLiteralVariant,
    encodeField,
    encodeFieldType,
    encodeFloatType,
    encodeFunction,
    encodeFunctionType,
    encodeIntegerType,
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


encodeApplication :: (Default m, Ord m) => Context m -> Application m -> Term m
encodeApplication cx (Application f a) = nominalRecord cx _Application [
  Field _Application_function $ encodeTerm cx f,
  Field _Application_argument $ encodeTerm cx f]

encodeLiteralType :: Default m => Context m -> LiteralType -> Term m
encodeLiteralType cx at = case at of
  LiteralTypeBinary -> nominalUnitVariant cx _LiteralType _LiteralType_binary
  LiteralTypeBoolean -> nominalUnitVariant cx _LiteralType _LiteralType_boolean
  LiteralTypeFloat ft -> nominalVariant cx _LiteralType _LiteralType_float $ encodeFloatType cx ft
  LiteralTypeInteger it -> nominalVariant cx _LiteralType _LiteralType_integer $ encodeIntegerType cx it
  LiteralTypeString -> nominalUnitVariant cx _LiteralType _LiteralType_string

encodeLiteral :: Default m => Context m -> Literal -> Term m
encodeLiteral cx = atomic

encodeLiteralVariant :: Default m => Context m -> LiteralVariant -> Term m
encodeLiteralVariant cx av = nominalUnitVariant cx _LiteralVariant $ case av of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

encodeField :: (Default m, Ord m) => Context m -> Field m -> Term m
encodeField cx (Field name term) = nominalRecord cx _Field [
  Field _Field_name $ stringValue name,
  Field _Field_term $ encodeTerm cx term]

encodeFieldType :: Default m => Context m -> FieldType -> Term m
encodeFieldType cx (FieldType fname t) = nominalRecord cx _FieldType [
  Field _FieldType_name $ stringValue fname,
  Field _FieldType_type $ encodeType cx t]

encodeFloatType :: Default m => Context m -> FloatType -> Term m
encodeFloatType cx ft = nominalUnitVariant cx _FloatType $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

encodeFunction :: (Default m, Ord m) => Context m -> Function m -> Term m
encodeFunction cx f = case f of
  FunctionCases cases -> nominalVariant cx _Function _Function_cases $ list $ encodeField cx <$> cases
  FunctionCompareTo other -> nominalVariant cx _Function _Function_compareTo $ encodeTerm cx other
  FunctionData -> nominalUnitVariant cx _Function _Function_data
  FunctionLambda l -> nominalVariant cx _Function _Function_lambda $ encodeLambda cx l
  FunctionOptionalCases cases -> nominalVariant cx _Function _Function_optionalCases $ encodeOptionalCases cx cases
  FunctionPrimitive name -> nominalVariant cx _Function _Function_primitive $ stringValue name
  FunctionProjection fname -> nominalVariant cx _Function _Function_projection $ stringValue fname

encodeFunctionType :: Default m => Context m -> FunctionType -> Term m
encodeFunctionType cx (FunctionType dom cod) = nominalRecord cx _FunctionType [
  Field _FunctionType_domain $ encodeType cx dom,
  Field _FunctionType_codomain $ encodeType cx cod]

encodeIntegerType :: Default m => Context m -> IntegerType -> Term m
encodeIntegerType cx it = nominalUnitVariant cx _IntegerType $ case it of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

encodeLambda :: (Default m, Ord m) => Context m -> Lambda m -> Term m
encodeLambda cx (Lambda v b) = nominalRecord cx _Lambda [
  Field _Lambda_parameter $ stringValue v,
  Field _Lambda_body $ encodeTerm cx b]

encodeMapType :: Default m => Context m -> MapType -> Term m
encodeMapType cx (MapType kt vt) = nominalRecord cx _MapType [
  Field _MapType_keys $ encodeType cx kt,
  Field _MapType_values $ encodeType cx vt]

encodeNominalTerm :: (Default m, Ord m) => Context m -> NominalTerm m -> Term m
encodeNominalTerm cx (NominalTerm name term) = nominalRecord cx _NominalTerm [
  Field _NominalTerm_typeName $ stringValue name,
  Field _NominalTerm_term $ encodeTerm cx term]

encodeOptionalCases :: (Default m, Ord m) => Context m -> OptionalCases m -> Term m
encodeOptionalCases cx (OptionalCases nothing just) = nominalRecord cx _OptionalCases [
  Field _OptionalCases_nothing $ encodeTerm cx nothing,
  Field _OptionalCases_just $ encodeTerm cx just]

encodeTerm :: (Default m, Ord m) => Context m -> Term m -> Term m
encodeTerm cx term = case termData term of
  ExpressionApplication a -> nominalVariant cx _Expression _Expression_application $ encodeApplication cx a
  ExpressionLiteral av -> nominalVariant cx _Expression _Expression_literal $ encodeLiteral cx av
  ExpressionElement name -> nominalVariant cx _Expression _Expression_element $ stringValue name
  ExpressionFunction f -> nominalVariant cx _Expression _Expression_function $ encodeFunction cx f
  ExpressionList terms -> nominalVariant cx _Expression _Expression_list $ list $ encodeTerm cx <$> terms
  ExpressionMap m -> nominalVariant cx _Expression _Expression_map $ map $ M.fromList $ encodePair <$> M.toList m
    where encodePair (k, v) = (encodeTerm cx k, encodeTerm cx v)
  ExpressionNominal ntt -> nominalVariant cx _Expression _Expression_nominal $ encodeNominalTerm cx ntt
  ExpressionOptional m -> nominalVariant cx _Expression _Expression_optional $ optional $ encodeTerm cx <$> m
  ExpressionRecord fields -> nominalVariant cx _Expression _Expression_record $ list $ encodeField cx <$> fields
  ExpressionSet terms -> nominalVariant cx _Expression _Expression_set $ set $ S.fromList $ encodeTerm cx <$> S.toList terms
  ExpressionUnion field -> nominalVariant cx _Expression _Expression_union $ encodeField cx field
  ExpressionVariable var -> nominalVariant cx _Expression _Expression_variable $ stringValue var

encodeType :: Default m => Context m -> Type -> Term m
encodeType cx typ = case typ of
  TypeLiteral at -> nominalVariant cx _Type _Type_literal $ encodeLiteralType cx at
  TypeElement t -> nominalVariant cx _Type _Type_element $ encodeType cx t
  TypeFunction ft -> nominalVariant cx _Type _Type_function $ encodeFunctionType cx ft
  TypeList t -> nominalVariant cx _Type _Type_list $ encodeType cx t
  TypeMap mt -> nominalVariant cx _Type _Type_map $ encodeMapType cx mt
  TypeNominal name -> nominalVariant cx _Type _Type_nominal $ element name
  TypeOptional t -> nominalVariant cx _Type _Type_optional $ encodeType cx t
  TypeRecord fields -> nominalVariant cx _Type _Type_record $ list $ fmap (encodeFieldType cx) fields
  TypeSet t -> nominalVariant cx _Type _Type_set $ encodeType cx t
  TypeUnion fields -> nominalVariant cx _Type _Type_union $ list $ fmap (encodeFieldType cx) fields
  TypeUniversal ut -> nominalVariant cx _Type _Type_universal $ encodeUniversalType cx ut
  TypeVariable var -> nominalVariant cx _Type _Type_variable $ stringValue var

encodeTypeVariant :: Default m => Context m -> TypeVariant -> Term m
encodeTypeVariant cx tv = nominalUnitVariant cx _TypeVariant $ case tv of
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

encodeUniversalType :: Default m => Context m -> UniversalType -> Term m
encodeUniversalType cx (UniversalType var body) = nominalRecord cx _UniversalType [
  Field _UniversalType_variable $ stringValue var,
  Field _UniversalType_body $ encodeType cx body]
