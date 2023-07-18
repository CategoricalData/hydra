-- | Encoding of types as terms (epsilon encoding)

module Hydra.CoreEncoding where

import Hydra.Basics
import Hydra.Core
import Hydra.Mantle
import Hydra.Dsl.Terms as Terms

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


epsilonEncodeApplicationType :: ApplicationType a -> Term a
epsilonEncodeApplicationType (ApplicationType lhs rhs) = record _ApplicationType [
  Field _ApplicationType_function $ epsilonEncodeType lhs,
  Field _ApplicationType_argument $ epsilonEncodeType rhs]

epsilonEncodeFieldType :: FieldType a -> Term a
epsilonEncodeFieldType (FieldType (FieldName fname) t) = record _FieldType [
  Field _FieldType_name $ string fname,
  Field _FieldType_type $ epsilonEncodeType t]

epsilonEncodeFloatType :: FloatType -> Term a
epsilonEncodeFloatType ft = unitVariant _FloatType $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

epsilonEncodeFunctionType :: FunctionType a -> Term a
epsilonEncodeFunctionType (FunctionType dom cod) = record _FunctionType [
  Field _FunctionType_domain $ epsilonEncodeType dom,
  Field _FunctionType_codomain $ epsilonEncodeType cod]

epsilonEncodeIntegerType :: IntegerType -> Term a
epsilonEncodeIntegerType it = unitVariant _IntegerType $ case it of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

epsilonEncodeLambdaType :: LambdaType a -> Term a
epsilonEncodeLambdaType (LambdaType var body) = record _LambdaType [
  Field _LambdaType_parameter $ epsilonEncodeName var,
  Field _LambdaType_body $ epsilonEncodeType body]

epsilonEncodeLiteralType :: LiteralType -> Term a
epsilonEncodeLiteralType at = case at of
  LiteralTypeBinary -> unitVariant _LiteralType _LiteralType_binary
  LiteralTypeBoolean -> unitVariant _LiteralType _LiteralType_boolean
  LiteralTypeFloat ft -> variant _LiteralType _LiteralType_float $ epsilonEncodeFloatType ft
  LiteralTypeInteger it -> variant _LiteralType _LiteralType_integer $ epsilonEncodeIntegerType it
  LiteralTypeString -> unitVariant _LiteralType _LiteralType_string

epsilonEncodeMapType :: MapType a -> Term a
epsilonEncodeMapType (MapType kt vt) = record _MapType [
  Field _MapType_keys $ epsilonEncodeType kt,
  Field _MapType_values $ epsilonEncodeType vt]

epsilonEncodeName :: Name -> Term a
epsilonEncodeName name = string $ unName name

epsilonEncodeNominal :: (x -> Term a) -> Nominal x -> Term a
epsilonEncodeNominal mapping (Nominal name obj) = record _Nominal [
  Field _Nominal_typeName $ epsilonEncodeName name,
  Field _Nominal_object $ mapping obj]

epsilonEncodeRowType :: RowType a -> Term a
epsilonEncodeRowType (RowType name extends fields) = record _RowType [
  Field _RowType_typeName $ string (unName name),
  Field _RowType_extends $ optional (string . unName <$> extends),
  Field _RowType_fields $ list $ epsilonEncodeFieldType <$> fields]

epsilonEncodeType :: Type a -> Term a
epsilonEncodeType typ = case typ of
    TypeAnnotated (Annotated t ann) -> TermAnnotated (Annotated (epsilonEncodeType t) ann)
    TypeApplication a -> tvar _Type_application $ epsilonEncodeApplicationType a
    TypeFunction ft -> tvar _Type_function $ epsilonEncodeFunctionType ft
    TypeLambda ut -> tvar _Type_lambda $ epsilonEncodeLambdaType ut
    TypeList t -> tvar _Type_list $ epsilonEncodeType t
    TypeLiteral at -> tvar _Type_literal $ epsilonEncodeLiteralType at
    TypeMap mt -> tvar _Type_map $ epsilonEncodeMapType mt
    TypeOptional t -> tvar _Type_optional $ epsilonEncodeType t
    TypeProduct types -> tvar _Type_product $ list (epsilonEncodeType <$> types)
    TypeRecord rt -> tvar _Type_record $ epsilonEncodeRowType rt
    TypeSet t -> tvar _Type_set $ epsilonEncodeType t
    TypeSum types -> tvar _Type_sum $ list (epsilonEncodeType <$> types)
    TypeUnion rt -> tvar _Type_union $ epsilonEncodeRowType rt
    TypeVariable name -> tvar _Type_variable $ epsilonEncodeName name
    TypeWrap n -> tvar _Type_wrap $ epsilonEncodeNominal epsilonEncodeType n
  where
    tvar = variant _Type
