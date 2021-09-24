module Hydra.Prototyping.TermEncoding (
    atomicTypeAsTerm,
    fieldTypeAsTerm,
    floatTypeAsTerm,
    functionTypeAsTerm,
    integerTypeAsTerm,
    typeAsTerm,
  ) where

import Hydra.Core
import Hydra.Prototyping.Helpers


atomicTypeAsTerm :: AtomicType -> Term
atomicTypeAsTerm at = case at of
  AtomicTypeBinary -> unitVariant _AtomicType_binary
  AtomicTypeBoolean -> unitVariant _AtomicType_boolean
  AtomicTypeFloat ft -> variant _AtomicType_float $ floatTypeAsTerm ft
  AtomicTypeInteger it -> variant _AtomicType_integer $ integerTypeAsTerm it
  AtomicTypeString -> unitVariant _AtomicType_string

fieldTypeAsTerm :: FieldType -> Term
fieldTypeAsTerm (FieldType fname t) = TermRecord [
  Field _FieldType_name $ stringTerm fname,
  Field _FieldType_type $ typeAsTerm t]

floatTypeAsTerm :: FloatType -> Term
floatTypeAsTerm ft = unitVariant $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

functionTypeAsTerm :: FunctionType -> Term
functionTypeAsTerm (FunctionType dom cod) = TermRecord [
  Field _FunctionType_domain $ typeAsTerm dom,
  Field _FunctionType_codomain $ typeAsTerm cod]

integerTypeAsTerm :: IntegerType -> Term
integerTypeAsTerm it = unitVariant $ case it of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

typeAsTerm :: Type -> Term
typeAsTerm typ = case typ of
  TypeAtomic at -> variant _Type_atomic $ atomicTypeAsTerm at
  TypeElement t -> variant _Type_element $ typeAsTerm t
  TypeFunction ft -> variant _Type_function $ functionTypeAsTerm ft
  TypeList t -> variant _Type_list $ typeAsTerm t
  TypeNominal name -> variant _Type_nominal $ stringTerm name
  TypeRecord fields -> variant _Type_record $ TermList $ fmap fieldTypeAsTerm fields
  TypeUnion fields -> variant _Type_union $ TermList $ fmap fieldTypeAsTerm fields
