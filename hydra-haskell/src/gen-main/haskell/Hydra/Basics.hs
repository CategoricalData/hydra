module Hydra.Basics where

import Hydra.Core as Core
import Hydra.Lib.Lists as Lists
import Hydra.Lib.Strings as Strings
import Data.Map
import Data.Set

-- Find the precision of a given floating-point type
floatTypePrecision :: (FloatType -> Precision)
floatTypePrecision x = case x of
  FloatTypeBigfloat -> PrecisionArbitrary
  FloatTypeFloat32 -> (PrecisionBits 32)
  FloatTypeFloat64 -> (PrecisionBits 64)

-- All floating-point types in a canonical order
floatTypes :: [FloatType]
floatTypes = [
  FloatTypeBigfloat,
  FloatTypeFloat32,
  FloatTypeFloat64]

-- Find the float type for a given floating-point value
floatValueType :: (FloatValue -> FloatType)
floatValueType x = case x of
  FloatValueBigfloat _ -> FloatTypeBigfloat
  FloatValueFloat32 _ -> FloatTypeFloat32
  FloatValueFloat64 _ -> FloatTypeFloat64

-- Find the function variant (constructor) for a given function
functionVariant :: (Function a -> FunctionVariant)
functionVariant x = case x of
  FunctionCases _ -> FunctionVariantCases
  FunctionCompareTo _ -> FunctionVariantCompareTo
  FunctionData -> FunctionVariantData
  FunctionLambda _ -> FunctionVariantLambda
  FunctionOptionalCases _ -> FunctionVariantOptionalCases
  FunctionPrimitive _ -> FunctionVariantPrimitive
  FunctionProjection _ -> FunctionVariantProjection

-- All function variants (constructors), in a canonical order
functionVariants :: [FunctionVariant]
functionVariants = [
  FunctionVariantCases,
  FunctionVariantCompareTo,
  FunctionVariantData,
  FunctionVariantLambda,
  FunctionVariantOptionalCases,
  FunctionVariantPrimitive,
  FunctionVariantProjection]

-- Find whether a given integer type is signed (true) or unsigned (false)
integerTypeIsSigned :: (IntegerType -> Bool)
integerTypeIsSigned x = case x of
  IntegerTypeBigint -> True
  IntegerTypeInt8 -> True
  IntegerTypeInt16 -> True
  IntegerTypeInt32 -> True
  IntegerTypeInt64 -> True
  IntegerTypeUint8 -> False
  IntegerTypeUint16 -> False
  IntegerTypeUint32 -> False
  IntegerTypeUint64 -> False

-- Find the precision of a given integer type
integerTypePrecision :: (IntegerType -> Precision)
integerTypePrecision x = case x of
  IntegerTypeBigint -> PrecisionArbitrary
  IntegerTypeInt8 -> (PrecisionBits 8)
  IntegerTypeInt16 -> (PrecisionBits 16)
  IntegerTypeInt32 -> (PrecisionBits 32)
  IntegerTypeInt64 -> (PrecisionBits 64)
  IntegerTypeUint8 -> (PrecisionBits 8)
  IntegerTypeUint16 -> (PrecisionBits 16)
  IntegerTypeUint32 -> (PrecisionBits 32)
  IntegerTypeUint64 -> (PrecisionBits 64)

-- All integer types, in a canonical order
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

-- Find the integer type for a given integer value
integerValueType :: (IntegerValue -> IntegerType)
integerValueType x = case x of
  IntegerValueBigint _ -> IntegerTypeBigint
  IntegerValueInt8 _ -> IntegerTypeInt8
  IntegerValueInt16 _ -> IntegerTypeInt16
  IntegerValueInt32 _ -> IntegerTypeInt32
  IntegerValueInt64 _ -> IntegerTypeInt64
  IntegerValueUint8 _ -> IntegerTypeUint8
  IntegerValueUint16 _ -> IntegerTypeUint16
  IntegerValueUint32 _ -> IntegerTypeUint32
  IntegerValueUint64 _ -> IntegerTypeUint64

-- Find the literal type for a given literal value
literalType :: (Literal -> LiteralType)
literalType x = case x of
  LiteralBinary _ -> LiteralTypeBinary
  LiteralBoolean _ -> LiteralTypeBoolean
  LiteralFloat v -> (LiteralTypeFloat (floatValueType v))
  LiteralInteger v -> (LiteralTypeInteger (integerValueType v))
  LiteralString _ -> LiteralTypeString

-- Find the literal type variant (constructor) for a given literal value
literalTypeVariant :: (LiteralType -> LiteralVariant)
literalTypeVariant x = case x of
  LiteralTypeBinary -> LiteralVariantBinary
  LiteralTypeBoolean -> LiteralVariantBoolean
  LiteralTypeFloat _ -> LiteralVariantFloat
  LiteralTypeInteger _ -> LiteralVariantInteger
  LiteralTypeString -> LiteralVariantString

-- Find the literal variant (constructor) for a given literal value
literalVariant :: (Literal -> LiteralVariant)
literalVariant x = (literalTypeVariant (literalType x))

-- All literal variants, in a canonical order
literalVariants :: [LiteralVariant]
literalVariants = [
  LiteralVariantBinary,
  LiteralVariantBoolean,
  LiteralVariantFloat,
  LiteralVariantInteger,
  LiteralVariantString]

-- Construct a qualified (dot-separated) name
qname :: (Name -> String -> Name)
qname ns name = (
  Strings.cat [
    ns,
    ".",
    name])

-- Find the term variant (constructor) for a given term
termVariant :: (Term a -> TermVariant)
termVariant term = (
  (
    \x -> case x of
      ExpressionApplication _ -> TermVariantApplication
      ExpressionElement _ -> TermVariantElement
      ExpressionFunction _ -> TermVariantFunction
      ExpressionList _ -> TermVariantList
      ExpressionLiteral _ -> TermVariantLiteral
      ExpressionMap _ -> TermVariantMap
      ExpressionNominal _ -> TermVariantNominal
      ExpressionOptional _ -> TermVariantOptional
      ExpressionRecord _ -> TermVariantRecord
      ExpressionSet _ -> TermVariantSet
      ExpressionTypeAbstraction _ -> TermVariantTypeAbstraction
      ExpressionTypeApplication _ -> TermVariantTypeApplication
      ExpressionUnion _ -> TermVariantUnion
      ExpressionVariable _ -> TermVariantVariable) (termData term))

-- All term (expression) variants, in a canonical order
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

-- TODO: temporary. Just a token polymorphic function for testing
testLists :: ([[a]] -> Int)
testLists els = (Lists.length (Lists.concat els))

-- Find the type variant (constructor) for a given type
typeVariant :: (Type -> TypeVariant)
typeVariant x = case x of
  TypeElement _ -> TypeVariantElement
  TypeFunction _ -> TypeVariantFunction
  TypeList _ -> TypeVariantList
  TypeLiteral _ -> TypeVariantLiteral
  TypeMap _ -> TypeVariantMap
  TypeNominal _ -> TypeVariantNominal
  TypeOptional _ -> TypeVariantOptional
  TypeRecord _ -> TypeVariantRecord
  TypeSet _ -> TypeVariantSet
  TypeUnion _ -> TypeVariantUnion
  TypeUniversal _ -> TypeVariantUniversal
  TypeVariable _ -> TypeVariantVariable

-- All type variants, in a canonical order
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