module Hydra.Prototyping.TermEncoding (
    atomicTypeAsTerm,
    atomicVariantAsTerm,
    fieldTypeAsTerm,
    floatTypeAsTerm,
    floatVariantAsTerm,
    functionTypeAsTerm,
    integerTypeAsTerm,
    integerVariantAsTerm,
    typeAsTerm,
    typeVariantAsTerm,
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

atomicVariantAsTerm :: AtomicVariant -> Term
atomicVariantAsTerm av = unitVariant $ case av of
  AtomicVariantBinary -> _AtomicVariant_binary
  AtomicVariantBoolean -> _AtomicVariant_boolean
  AtomicVariantFloat -> _AtomicVariant_float
  AtomicVariantInteger -> _AtomicVariant_integer
  AtomicVariantString -> _AtomicVariant_string

fieldTypeAsTerm :: FieldType -> Term
fieldTypeAsTerm (FieldType fname t) = TermRecord [
  Field _FieldType_name $ stringTerm fname,
  Field _FieldType_type $ typeAsTerm t]

floatTypeAsTerm :: FloatType -> Term
floatTypeAsTerm ft = unitVariant $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

floatVariantAsTerm :: FloatVariant -> Term
floatVariantAsTerm fv = unitVariant $ case fv of
  FloatVariantBigfloat -> _FloatVariant_bigfloat
  FloatVariantFloat32 -> _FloatVariant_float32
  FloatVariantFloat64 -> _FloatVariant_float64

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

integerVariantAsTerm :: IntegerVariant -> Term
integerVariantAsTerm iv = unitVariant $ case iv of
  IntegerVariantBigint -> _IntegerType_bigint
  IntegerVariantInt8 -> _IntegerType_int8
  IntegerVariantInt16 -> _IntegerType_int16
  IntegerVariantInt32 -> _IntegerType_int32
  IntegerVariantInt64 -> _IntegerType_int64
  IntegerVariantUint8 -> _IntegerType_uint8
  IntegerVariantUint16 -> _IntegerType_uint16
  IntegerVariantUint32 -> _IntegerType_uint32
  IntegerVariantUint64 -> _IntegerType_uint64

mapTypeAsTerm :: MapType -> Term
mapTypeAsTerm (MapType kt vt) = TermRecord [
  Field _MapType_keys $ typeAsTerm kt,
  Field _MapType_values $ typeAsTerm vt]

typeAsTerm :: Type -> Term
typeAsTerm typ = case typ of
  TypeAtomic at -> variant _Type_atomic $ atomicTypeAsTerm at
  TypeElement t -> variant _Type_element $ typeAsTerm t
  TypeFunction ft -> variant _Type_function $ functionTypeAsTerm ft
  TypeList t -> variant _Type_list $ typeAsTerm t
  TypeMap mt -> variant _Type_map $ mapTypeAsTerm mt
  TypeNominal name -> variant _Type_nominal $ stringTerm name
  TypeRecord fields -> variant _Type_record $ TermList $ fmap fieldTypeAsTerm fields
  TypeSet t -> variant _Type_set $ typeAsTerm t
  TypeUnion fields -> variant _Type_union $ TermList $ fmap fieldTypeAsTerm fields

typeVariantAsTerm :: TypeVariant -> Term
typeVariantAsTerm tv = unitVariant $ case tv of
  TypeVariantAtomic -> _TypeVariant_atomic
  TypeVariantElement -> _TypeVariant_element
  TypeVariantFunction -> _TypeVariant_function
  TypeVariantList -> _TypeVariant_list
  TypeVariantMap -> _TypeVariant_map
  TypeVariantNominal -> _TypeVariant_nominal
  TypeVariantRecord -> _TypeVariant_record
  TypeVariantSet -> _TypeVariant_set
  TypeVariantUnion -> _TypeVariant_union
