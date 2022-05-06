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
  Core.FunctionDelta -> Core.FunctionVariantDelta
  Core.FunctionLambda _ -> Core.FunctionVariantLambda
  Core.FunctionOptionalCases _ -> Core.FunctionVariantOptionalCases
  Core.FunctionPrimitive _ -> Core.FunctionVariantPrimitive
  Core.FunctionProjection _ -> Core.FunctionVariantProjection

-- All function variants (constructors), in a canonical order
functionVariants :: [Core.FunctionVariant]
functionVariants = [
  Core.FunctionVariantCases,
  Core.FunctionVariantCompareTo,
  Core.FunctionVariantDelta,
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
termVariant :: (Core.Data m -> Core.DataVariant)
termVariant term = (
  (
    \x -> case x of
      Core.DataTermApplication _ -> Core.DataVariantApplication
      Core.DataTermElement _ -> Core.DataVariantElement
      Core.DataTermFunction _ -> Core.DataVariantFunction
      Core.DataTermList _ -> Core.DataVariantList
      Core.DataTermLiteral _ -> Core.DataVariantLiteral
      Core.DataTermMap _ -> Core.DataVariantMap
      Core.DataTermNominal _ -> Core.DataVariantNominal
      Core.DataTermOptional _ -> Core.DataVariantOptional
      Core.DataTermRecord _ -> Core.DataVariantRecord
      Core.DataTermSet _ -> Core.DataVariantSet
      Core.DataTermTypeAbstraction _ -> Core.DataVariantTypeAbstraction
      Core.DataTermTypeApplication _ -> Core.DataVariantTypeApplication
      Core.DataTermUnion _ -> Core.DataVariantUnion
      Core.DataTermVariable _ -> Core.DataVariantVariable) (Core.dataTerm term))

-- All term (expression) variants, in a canonical order
termVariants :: [Core.DataVariant]
termVariants = [
  Core.DataVariantApplication,
  Core.DataVariantLiteral,
  Core.DataVariantElement,
  Core.DataVariantFunction,
  Core.DataVariantList,
  Core.DataVariantMap,
  Core.DataVariantNominal,
  Core.DataVariantOptional,
  Core.DataVariantRecord,
  Core.DataVariantSet,
  Core.DataVariantUnion,
  Core.DataVariantVariable]

-- TODO: temporary. Just a token polymorphic function for testing
testLists :: ([[a]] -> Int)
testLists els = (Lists.length (Lists.concat els))

-- Find the type variant (constructor) for a given type
typeVariant :: (Core.Type m -> Core.TypeVariant)
typeVariant typ = (
  (
    \x -> case x of
      Core.TypeTermElement _ -> Core.TypeVariantElement
      Core.TypeTermFunction _ -> Core.TypeVariantFunction
      Core.TypeTermList _ -> Core.TypeVariantList
      Core.TypeTermLiteral _ -> Core.TypeVariantLiteral
      Core.TypeTermMap _ -> Core.TypeVariantMap
      Core.TypeTermNominal _ -> Core.TypeVariantNominal
      Core.TypeTermOptional _ -> Core.TypeVariantOptional
      Core.TypeTermRecord _ -> Core.TypeVariantRecord
      Core.TypeTermSet _ -> Core.TypeVariantSet
      Core.TypeTermUnion _ -> Core.TypeVariantUnion
      Core.TypeTermUniversal _ -> Core.TypeVariantUniversal
      Core.TypeTermVariable _ -> Core.TypeVariantVariable) (Core.typeTerm typ))

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