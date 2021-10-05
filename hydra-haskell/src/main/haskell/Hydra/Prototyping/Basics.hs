module Hydra.Prototyping.Basics (
    atomicTypeVariant,
    atomicValueType,
    atomicValueVariant,
    atomicVariants,
    floatTypeVariant,
    floatValueType,
    floatValueVariant,
    floatVariantPrecision,
    floatVariants,
    hydraCoreLanguage,
    integerTypeVariant,
    integerValueType,
    integerValueVariant,
    integerVariantIsSigned,
    integerVariantPrecision,
    integerVariants,
    termVariant,
    termVariants,
    typeVariant,
    typeVariants,
  ) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Ext.Haskell.Dsl
import Hydra.Translation

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.Maybe as Y


atomicTypeVariant :: AtomicType -> AtomicVariant
atomicTypeVariant at = case at of
  AtomicTypeBinary -> AtomicVariantBinary
  AtomicTypeBoolean -> AtomicVariantBoolean
  AtomicTypeFloat _ -> AtomicVariantFloat
  AtomicTypeInteger _ -> AtomicVariantInteger
  AtomicTypeString -> AtomicVariantString

atomicValueType :: AtomicValue -> AtomicType
atomicValueType v = case v of
  AtomicValueBinary _ -> AtomicTypeBinary
  AtomicValueBoolean _ -> AtomicTypeBoolean
  AtomicValueFloat fv -> AtomicTypeFloat $ floatValueType fv
  AtomicValueInteger iv -> AtomicTypeInteger $ integerValueType iv
  AtomicValueString _ -> AtomicTypeString

atomicValueVariant :: AtomicValue -> AtomicVariant
atomicValueVariant = atomicTypeVariant . atomicValueType

atomicVariants :: [AtomicVariant]
atomicVariants = [
  AtomicVariantBinary, AtomicVariantBoolean, AtomicVariantFloat, AtomicVariantInteger, AtomicVariantString]

floatTypeVariant :: FloatType -> FloatVariant
floatTypeVariant ft = case ft of
  FloatTypeBigfloat -> FloatVariantBigfloat
  FloatTypeFloat32 -> FloatVariantFloat32
  FloatTypeFloat64 -> FloatVariantFloat64

floatValueType :: FloatValue -> FloatType
floatValueType fv = case fv of
  FloatValueBigfloat _ -> FloatTypeBigfloat
  FloatValueFloat32 _ -> FloatTypeFloat32
  FloatValueFloat64 _ -> FloatTypeFloat64

floatValueVariant :: FloatValue -> FloatVariant
floatValueVariant = floatTypeVariant . floatValueType

floatVariantPrecision :: FloatVariant -> Precision
floatVariantPrecision v = case v of
  FloatVariantBigfloat -> PrecisionArbitrary
  FloatVariantFloat32 -> PrecisionBits 32
  FloatVariantFloat64 -> PrecisionBits 64

floatVariants :: [FloatVariant]
floatVariants = [FloatVariantFloat32, FloatVariantFloat64, FloatVariantBigfloat]

hydraCoreLanguage = Language "hydra/core" $ Language_Constraints {
  languageConstraintsAtomicVariants = S.fromList atomicVariants,
  languageConstraintsTermVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList typeVariants }

integerTypeVariant :: IntegerType -> IntegerVariant
integerTypeVariant it = case it of
  IntegerTypeBigint -> IntegerVariantBigint
  IntegerTypeInt8 -> IntegerVariantInt8
  IntegerTypeInt16 -> IntegerVariantInt16
  IntegerTypeInt32 -> IntegerVariantInt32
  IntegerTypeInt64 -> IntegerVariantInt64
  IntegerTypeUint8 -> IntegerVariantUint8
  IntegerTypeUint16 -> IntegerVariantUint16
  IntegerTypeUint32 -> IntegerVariantUint32
  IntegerTypeUint64 -> IntegerVariantUint64

integerValueType :: IntegerValue -> IntegerType
integerValueType iv = case iv of
  IntegerValueBigint _ -> IntegerTypeBigint
  IntegerValueInt8 _ -> IntegerTypeInt8
  IntegerValueInt16 _ -> IntegerTypeInt16
  IntegerValueInt32 _ -> IntegerTypeInt32
  IntegerValueInt64 _ -> IntegerTypeInt64
  IntegerValueUint8 _ -> IntegerTypeUint8
  IntegerValueUint16 _ -> IntegerTypeUint16
  IntegerValueUint32 _ -> IntegerTypeUint32
  IntegerValueUint64 _ -> IntegerTypeUint64

integerValueVariant :: IntegerValue -> IntegerVariant
integerValueVariant = integerTypeVariant . integerValueType

integerVariantIsSigned :: IntegerVariant -> Bool
integerVariantIsSigned v = case v of
  IntegerVariantUint8 -> False
  IntegerVariantUint16 -> False
  IntegerVariantUint32 -> False
  IntegerVariantUint64 -> False
  _ -> True

integerVariantPrecision :: IntegerVariant -> Precision
integerVariantPrecision v = case v of
  IntegerVariantBigint -> PrecisionArbitrary
  IntegerVariantInt8 -> PrecisionBits 8
  IntegerVariantInt16 -> PrecisionBits 16
  IntegerVariantInt32 -> PrecisionBits 32
  IntegerVariantInt64 -> PrecisionBits 64
  IntegerVariantUint8 -> PrecisionBits 8
  IntegerVariantUint16 -> PrecisionBits 16
  IntegerVariantUint32 -> PrecisionBits 32
  IntegerVariantUint64 -> PrecisionBits 64
  
integerVariants :: [IntegerVariant]
integerVariants = [
  IntegerVariantInt8, IntegerVariantInt16, IntegerVariantInt32, IntegerVariantInt64,
  IntegerVariantUint8, IntegerVariantUint16, IntegerVariantUint32, IntegerVariantUint64,
  IntegerVariantBigint]

termVariant :: Term -> TermVariant
termVariant term = case term of
  TermApplication _ -> TermVariantApplication
  TermAtomic _ -> TermVariantAtomic
  TermCases _ -> TermVariantCases
  TermCompareTo _ -> TermVariantCompareTo
  TermData -> TermVariantData
  TermElement _ -> TermVariantElement
  TermFunction _ -> TermVariantFunction
  TermLambda _ -> TermVariantLambda
  TermList _ -> TermVariantList
  TermMap _ -> TermVariantMap
  TermProjection _ -> TermVariantProjection
  TermRecord _ -> TermVariantRecord
  TermSet _ -> TermVariantSet
  TermUnion _ -> TermVariantUnion
  TermVariable _ -> TermVariantVariable

termVariants :: [TermVariant]
termVariants = [
  TermVariantApplication,
  TermVariantAtomic,
  TermVariantCases,
  TermVariantCompareTo,
  TermVariantData,
  TermVariantElement,
  TermVariantFunction,
  TermVariantLambda,
  TermVariantList,
  TermVariantMap,
  TermVariantProjection,
  TermVariantRecord,
  TermVariantSet,
  TermVariantUnion,
  TermVariantVariable]

typeVariant :: Type -> TypeVariant
typeVariant typ = case typ of
  TypeAtomic _ -> TypeVariantAtomic
  TypeElement _ -> TypeVariantElement
  TypeFunction _ -> TypeVariantFunction
  TypeList _ -> TypeVariantList
  TypeMap _ -> TypeVariantMap
  TypeNominal _ -> TypeVariantNominal
  TypeRecord _ -> TypeVariantRecord
  TypeSet _ -> TypeVariantSet
  TypeUnion _ -> TypeVariantUnion

typeVariants :: [TypeVariant]
typeVariants = [
  TypeVariantAtomic,
  TypeVariantElement,
  TypeVariantFunction,
  TypeVariantList,
  TypeVariantMap,
  TypeVariantNominal,
  TypeVariantRecord,
  TypeVariantSet,
  TypeVariantUnion]
