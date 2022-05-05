module Hydra.Basics where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Strings as Strings
import Data.Map
import Data.Set

-- Find the precision of a given floating-point type
floatTypePrecision :: (Core.FloatType -> Core.Precision)
floatTypePrecision x = case x of
  Core.FloatTypeBigfloat -> Core.PrecisionArbitrary
  Core.FloatTypeFloat32 -> (Core.PrecisionBits 32)
  Core.FloatTypeFloat64 -> (Core.PrecisionBits 64)

-- All floating-point types in a canonical order
floatTypes :: [Core.FloatType]
floatTypes = [
  Core.FloatTypeBigfloat,
  Core.FloatTypeFloat32,
  Core.FloatTypeFloat64]

-- Find the float type for a given floating-point value
floatValueType :: (Core.FloatValue -> Core.FloatType)
floatValueType x = case x of
  Core.FloatValueBigfloat _ -> Core.FloatTypeBigfloat
  Core.FloatValueFloat32 _ -> Core.FloatTypeFloat32
  Core.FloatValueFloat64 _ -> Core.FloatTypeFloat64

-- Find the function variant (constructor) for a given function
functionVariant :: (Core.Function m -> Core.FunctionVariant)
functionVariant x = case x of
  Core.FunctionCases _ -> Core.FunctionVariantCases
  Core.FunctionCompareTo _ -> Core.FunctionVariantCompareTo
  Core.FunctionData -> Core.FunctionVariantData
  Core.FunctionLambda _ -> Core.FunctionVariantLambda
  Core.FunctionOptionalCases _ -> Core.FunctionVariantOptionalCases
  Core.FunctionPrimitive _ -> Core.FunctionVariantPrimitive
  Core.FunctionProjection _ -> Core.FunctionVariantProjection

-- All function variants (constructors), in a canonical order
functionVariants :: [Core.FunctionVariant]
functionVariants = [
  Core.FunctionVariantCases,
  Core.FunctionVariantCompareTo,
  Core.FunctionVariantData,
  Core.FunctionVariantLambda,
  Core.FunctionVariantOptionalCases,
  Core.FunctionVariantPrimitive,
  Core.FunctionVariantProjection]

-- Find whether a given integer type is signed (true) or unsigned (false)
integerTypeIsSigned :: (Core.IntegerType -> Bool)
integerTypeIsSigned x = case x of
  Core.IntegerTypeBigint -> True
  Core.IntegerTypeInt8 -> True
  Core.IntegerTypeInt16 -> True
  Core.IntegerTypeInt32 -> True
  Core.IntegerTypeInt64 -> True
  Core.IntegerTypeUint8 -> False
  Core.IntegerTypeUint16 -> False
  Core.IntegerTypeUint32 -> False
  Core.IntegerTypeUint64 -> False

-- Find the precision of a given integer type
integerTypePrecision :: (Core.IntegerType -> Core.Precision)
integerTypePrecision x = case x of
  Core.IntegerTypeBigint -> Core.PrecisionArbitrary
  Core.IntegerTypeInt8 -> (Core.PrecisionBits 8)
  Core.IntegerTypeInt16 -> (Core.PrecisionBits 16)
  Core.IntegerTypeInt32 -> (Core.PrecisionBits 32)
  Core.IntegerTypeInt64 -> (Core.PrecisionBits 64)
  Core.IntegerTypeUint8 -> (Core.PrecisionBits 8)
  Core.IntegerTypeUint16 -> (Core.PrecisionBits 16)
  Core.IntegerTypeUint32 -> (Core.PrecisionBits 32)
  Core.IntegerTypeUint64 -> (Core.PrecisionBits 64)

-- All integer types, in a canonical order
integerTypes :: [Core.IntegerType]
integerTypes = [
  Core.IntegerTypeBigint,
  Core.IntegerTypeInt8,
  Core.IntegerTypeInt16,
  Core.IntegerTypeInt32,
  Core.IntegerTypeInt64,
  Core.IntegerTypeUint8,
  Core.IntegerTypeUint16,
  Core.IntegerTypeUint32,
  Core.IntegerTypeUint64]

-- Find the integer type for a given integer value
integerValueType :: (Core.IntegerValue -> Core.IntegerType)
integerValueType x = case x of
  Core.IntegerValueBigint _ -> Core.IntegerTypeBigint
  Core.IntegerValueInt8 _ -> Core.IntegerTypeInt8
  Core.IntegerValueInt16 _ -> Core.IntegerTypeInt16
  Core.IntegerValueInt32 _ -> Core.IntegerTypeInt32
  Core.IntegerValueInt64 _ -> Core.IntegerTypeInt64
  Core.IntegerValueUint8 _ -> Core.IntegerTypeUint8
  Core.IntegerValueUint16 _ -> Core.IntegerTypeUint16
  Core.IntegerValueUint32 _ -> Core.IntegerTypeUint32
  Core.IntegerValueUint64 _ -> Core.IntegerTypeUint64

-- Find the literal type for a given literal value
literalType :: (Core.Literal -> Core.LiteralType)
literalType x = case x of
  Core.LiteralBinary _ -> Core.LiteralTypeBinary
  Core.LiteralBoolean _ -> Core.LiteralTypeBoolean
  Core.LiteralFloat v -> (Core.LiteralTypeFloat (floatValueType v))
  Core.LiteralInteger v -> (Core.LiteralTypeInteger (integerValueType v))
  Core.LiteralString _ -> Core.LiteralTypeString

-- Find the literal type variant (constructor) for a given literal value
literalTypeVariant :: (Core.LiteralType -> Core.LiteralVariant)
literalTypeVariant x = case x of
  Core.LiteralTypeBinary -> Core.LiteralVariantBinary
  Core.LiteralTypeBoolean -> Core.LiteralVariantBoolean
  Core.LiteralTypeFloat _ -> Core.LiteralVariantFloat
  Core.LiteralTypeInteger _ -> Core.LiteralVariantInteger
  Core.LiteralTypeString -> Core.LiteralVariantString

-- Find the literal variant (constructor) for a given literal value
literalVariant :: (Core.Literal -> Core.LiteralVariant)
literalVariant x = (literalTypeVariant (literalType x))

-- All literal variants, in a canonical order
literalVariants :: [Core.LiteralVariant]
literalVariants = [
  Core.LiteralVariantBinary,
  Core.LiteralVariantBoolean,
  Core.LiteralVariantFloat,
  Core.LiteralVariantInteger,
  Core.LiteralVariantString]

-- Construct a qualified (dot-separated) name
qname :: (Core.Name -> String -> Core.Name)
qname ns name = (
  Strings.cat [
    ns,
    ".",
    name])

-- Find the term variant (constructor) for a given term
termVariant :: (Core.Term m -> Core.TermVariant)
termVariant term = (
  (
    \x -> case x of
      Core.ExpressionApplication _ -> Core.TermVariantApplication
      Core.ExpressionElement _ -> Core.TermVariantElement
      Core.ExpressionFunction _ -> Core.TermVariantFunction
      Core.ExpressionList _ -> Core.TermVariantList
      Core.ExpressionLiteral _ -> Core.TermVariantLiteral
      Core.ExpressionMap _ -> Core.TermVariantMap
      Core.ExpressionNominal _ -> Core.TermVariantNominal
      Core.ExpressionOptional _ -> Core.TermVariantOptional
      Core.ExpressionRecord _ -> Core.TermVariantRecord
      Core.ExpressionSet _ -> Core.TermVariantSet
      Core.ExpressionTypeAbstraction _ -> Core.TermVariantTypeAbstraction
      Core.ExpressionTypeApplication _ -> Core.TermVariantTypeApplication
      Core.ExpressionUnion _ -> Core.TermVariantUnion
      Core.ExpressionVariable _ -> Core.TermVariantVariable) (Core.termData term))

-- All term (expression) variants, in a canonical order
termVariants :: [Core.TermVariant]
termVariants = [
  Core.TermVariantApplication,
  Core.TermVariantLiteral,
  Core.TermVariantElement,
  Core.TermVariantFunction,
  Core.TermVariantList,
  Core.TermVariantMap,
  Core.TermVariantNominal,
  Core.TermVariantOptional,
  Core.TermVariantRecord,
  Core.TermVariantSet,
  Core.TermVariantUnion,
  Core.TermVariantVariable]

-- TODO: temporary. Just a token polymorphic function for testing
testLists :: ([[a]] -> Int)
testLists els = (Lists.length (Lists.concat els))

-- Find the type variant (constructor) for a given type
typeVariant :: (Core.Type m -> Core.TypeVariant)
typeVariant type = (
  (
    \x -> case x of
      Core.TypeExprElement _ -> Core.TypeVariantElement
      Core.TypeExprFunction _ -> Core.TypeVariantFunction
      Core.TypeExprList _ -> Core.TypeVariantList
      Core.TypeExprLiteral _ -> Core.TypeVariantLiteral
      Core.TypeExprMap _ -> Core.TypeVariantMap
      Core.TypeExprNominal _ -> Core.TypeVariantNominal
      Core.TypeExprOptional _ -> Core.TypeVariantOptional
      Core.TypeExprRecord _ -> Core.TypeVariantRecord
      Core.TypeExprSet _ -> Core.TypeVariantSet
      Core.TypeExprUnion _ -> Core.TypeVariantUnion
      Core.TypeExprUniversal _ -> Core.TypeVariantUniversal
      Core.TypeExprVariable _ -> Core.TypeVariantVariable) (Core.typeData type))

-- All type variants, in a canonical order
typeVariants :: [Core.TypeVariant]
typeVariants = [
  Core.TypeVariantLiteral,
  Core.TypeVariantElement,
  Core.TypeVariantFunction,
  Core.TypeVariantList,
  Core.TypeVariantMap,
  Core.TypeVariantNominal,
  Core.TypeVariantOptional,
  Core.TypeVariantRecord,
  Core.TypeVariantSet,
  Core.TypeVariantUnion,
  Core.TypeVariantUniversal,
  Core.TypeVariantVariable]