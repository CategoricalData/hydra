module Hydra.Prototyping.Basics (
    comparePrecision,
    floatTypePrecision,
    floatValueType,
    floatTypes,
    functionVariant,
    functionVariants,
    hydraCoreLanguage,
    integerTypeIsSigned,
    integerTypePrecision,
    integerTypes,
    integerValueType,
    literalTypeVariant,
    literalType,
    literalVariant,
    literalVariants,
    termVariant,
    termVariants,
    typeVariant,
    typeVariants,
  ) where

import Hydra.Core
import Hydra.Adapter

import qualified Data.Set as S


comparePrecision :: Precision -> Precision -> Ordering
comparePrecision p1 p2 = if p1 == p2 then EQ else case (p1, p2) of
  (PrecisionArbitrary, _) -> GT
  (_, PrecisionArbitrary) -> LT
  (PrecisionBits b1, PrecisionBits b2) -> compare b1 b2

floatValueType :: FloatValue -> FloatType
floatValueType fv = case fv of
  FloatValueBigfloat _ -> FloatTypeBigfloat
  FloatValueFloat32 _ -> FloatTypeFloat32
  FloatValueFloat64 _ -> FloatTypeFloat64

floatTypePrecision :: FloatType -> Precision
floatTypePrecision v = case v of
  FloatTypeBigfloat -> PrecisionArbitrary
  FloatTypeFloat32 -> PrecisionBits 32
  FloatTypeFloat64 -> PrecisionBits 64

floatTypes :: [FloatType]
floatTypes = [FloatTypeFloat32, FloatTypeFloat64, FloatTypeBigfloat]

functionVariant :: Function a -> FunctionVariant
functionVariant f = case f of
  FunctionCases _ -> FunctionVariantCases
  FunctionCompareTo _ -> FunctionVariantCompareTo
  FunctionData -> FunctionVariantData
  FunctionPrimitive _ -> FunctionVariantPrimitive
  FunctionProjection _ -> FunctionVariantProjection

functionVariants :: [FunctionVariant]
functionVariants = [
  FunctionVariantCases,
  FunctionVariantCompareTo,
  FunctionVariantData,
  FunctionVariantPrimitive,
  FunctionVariantLambda,
  FunctionVariantProjection]

hydraCoreLanguage :: Language
hydraCoreLanguage = Language "hydra/core" $ Language_Constraints {
  languageConstraintsLiteralVariants = S.fromList literalVariants,
  languageConstraintsFloatTypes = S.fromList floatTypes,
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerTypes = S.fromList integerTypes,
  languageConstraintsTermVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList typeVariants,
  languageConstraintsTypes = const True }

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

integerTypeIsSigned :: IntegerType -> Bool
integerTypeIsSigned v = case v of
  IntegerTypeUint8 -> False
  IntegerTypeUint16 -> False
  IntegerTypeUint32 -> False
  IntegerTypeUint64 -> False
  _ -> True

integerTypePrecision :: IntegerType -> Precision
integerTypePrecision v = case v of
  IntegerTypeBigint -> PrecisionArbitrary
  IntegerTypeInt8 -> PrecisionBits 8
  IntegerTypeInt16 -> PrecisionBits 16
  IntegerTypeInt32 -> PrecisionBits 32
  IntegerTypeInt64 -> PrecisionBits 64
  IntegerTypeUint8 -> PrecisionBits 8
  IntegerTypeUint16 -> PrecisionBits 16
  IntegerTypeUint32 -> PrecisionBits 32
  IntegerTypeUint64 -> PrecisionBits 64

integerTypes :: [IntegerType]
integerTypes = [
  IntegerTypeBigint,
  IntegerTypeInt8,
  IntegerTypeInt16,
  IntegerTypeInt32,
  IntegerTypeInt64,
  IntegerTypeUint8,
  IntegerTypeUint16,
  IntegerTypeUint32,
  IntegerTypeUint64]

literalType :: Literal -> LiteralType
literalType v = case v of
  LiteralBinary _ -> LiteralTypeBinary
  LiteralBoolean _ -> LiteralTypeBoolean
  LiteralFloat fv -> LiteralTypeFloat $ floatValueType fv
  LiteralInteger iv -> LiteralTypeInteger $ integerValueType iv
  LiteralString _ -> LiteralTypeString

literalTypeVariant :: LiteralType -> LiteralVariant
literalTypeVariant at = case at of
  LiteralTypeBinary -> LiteralVariantBinary
  LiteralTypeBoolean -> LiteralVariantBoolean
  LiteralTypeFloat _ -> LiteralVariantFloat
  LiteralTypeInteger _ -> LiteralVariantInteger
  LiteralTypeString -> LiteralVariantString

literalVariant :: Literal -> LiteralVariant
literalVariant = literalTypeVariant . literalType

literalVariants :: [LiteralVariant]
literalVariants = [
  LiteralVariantBinary,
  LiteralVariantBoolean,
  LiteralVariantFloat,
  LiteralVariantInteger,
  LiteralVariantString]

termVariant :: Term a -> TermVariant
termVariant term = case termData term of
  ExpressionApplication _ -> TermVariantApplication
  ExpressionLiteral _ -> TermVariantLiteral
  ExpressionElement _ -> TermVariantElement
  ExpressionFunction _ -> TermVariantFunction
  ExpressionList _ -> TermVariantList
  ExpressionMap _ -> TermVariantMap
  ExpressionNominal _ -> TermVariantNominal
  ExpressionOptional _ -> TermVariantOptional
  ExpressionRecord _ -> TermVariantRecord
  ExpressionSet _ -> TermVariantSet
  ExpressionTypeAbstraction _ -> TermVariantTypeAbstraction
  ExpressionTypeApplication _ -> TermVariantTypeApplication
  ExpressionUnion _ -> TermVariantUnion
  ExpressionVariable _ -> TermVariantVariable

termVariants :: [TermVariant]
termVariants = [
  TermVariantApplication,
  TermVariantLiteral,
  TermVariantElement,
  TermVariantFunction,
  TermVariantList,
  TermVariantMap,
  TermVariantNominal,
  TermVariantOptional,
  TermVariantRecord,
  TermVariantSet,
  TermVariantUnion,
  TermVariantVariable]

typeVariant :: Type -> TypeVariant
typeVariant typ = case typ of
  TypeLiteral _ -> TypeVariantLiteral
  TypeElement _ -> TypeVariantElement
  TypeFunction _ -> TypeVariantFunction
  TypeList _ -> TypeVariantList
  TypeMap _ -> TypeVariantMap
  TypeNominal _ -> TypeVariantNominal
  TypeOptional _ -> TypeVariantOptional
  TypeRecord _ -> TypeVariantRecord
  TypeSet _ -> TypeVariantSet
  TypeUnion _ -> TypeVariantUnion
  TypeVariable _ -> TypeVariantVariable
  TypeUniversal _ -> TypeVariantUniversal

typeVariants :: [TypeVariant]
typeVariants = [
  TypeVariantLiteral,
  TypeVariantElement,
  TypeVariantFunction,
  TypeVariantList,
  TypeVariantMap,
  TypeVariantNominal,
  TypeVariantOptional,
  TypeVariantRecord,
  TypeVariantSet,
  TypeVariantUnion,
  TypeVariantUniversal,
  TypeVariantVariable]
