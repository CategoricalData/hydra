module Hydra.Basics where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Strings as Strings
import Data.Map
import Data.Set

-- Find the elimination variant (constructor) for a given elimination term
eliminationVariant :: (Core.Elimination m -> Core.EliminationVariant)
eliminationVariant x = case x of
  Core.EliminationElement -> Core.EliminationVariantElement
  Core.EliminationNominal _ -> Core.EliminationVariantNominal
  Core.EliminationOptional _ -> Core.EliminationVariantOptional
  Core.EliminationRecord _ -> Core.EliminationVariantRecord
  Core.EliminationUnion _ -> Core.EliminationVariantUnion

-- All elimination variants (constructors), in a canonical order
eliminationVariants :: [Core.EliminationVariant]
eliminationVariants = [
  Core.EliminationVariantElement,
  Core.EliminationVariantNominal,
  Core.EliminationVariantOptional,
  Core.EliminationVariantRecord,
  Core.EliminationVariantUnion]

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
  Core.FunctionCompareTo _ -> Core.FunctionVariantCompareTo
  Core.FunctionElimination _ -> Core.FunctionVariantElimination
  Core.FunctionLambda _ -> Core.FunctionVariantLambda
  Core.FunctionPrimitive _ -> Core.FunctionVariantPrimitive

-- All function variants (constructors), in a canonical order
functionVariants :: [Core.FunctionVariant]
functionVariants = [
  Core.FunctionVariantCompareTo,
  Core.FunctionVariantElimination,
  Core.FunctionVariantLambda,
  Core.FunctionVariantPrimitive]

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
  Core.LiteralFloat v -> ((\x -> Core.LiteralTypeFloat x) (floatValueType v))
  Core.LiteralInteger v -> ((\x -> Core.LiteralTypeInteger x) (integerValueType v))
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
qname :: (Graph.GraphName -> String -> Core.Name)
qname ns name = (Core.Name (Strings.cat [
  Graph.unGraphName ns,
  ".",
  name]))

-- Find the term variant (constructor) for a given term
termVariant :: (Core.Term m -> Core.TermVariant)
termVariant term = ((\x -> case x of
  Core.TermAnnotated _ -> Core.TermVariantAnnotated
  Core.TermApplication _ -> Core.TermVariantApplication
  Core.TermElement _ -> Core.TermVariantElement
  Core.TermFunction _ -> Core.TermVariantFunction
  Core.TermLet _ -> Core.TermVariantLet
  Core.TermList _ -> Core.TermVariantList
  Core.TermLiteral _ -> Core.TermVariantLiteral
  Core.TermMap _ -> Core.TermVariantMap
  Core.TermNominal _ -> Core.TermVariantNominal
  Core.TermOptional _ -> Core.TermVariantOptional
  Core.TermRecord _ -> Core.TermVariantRecord
  Core.TermSet _ -> Core.TermVariantSet
  Core.TermUnion _ -> Core.TermVariantUnion
  Core.TermVariable _ -> Core.TermVariantVariable) term)

-- All term (expression) variants, in a canonical order
termVariants :: [Core.TermVariant]
termVariants = [
  Core.TermVariantAnnotated,
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
typeVariant typ = ((\x -> case x of
  Core.TypeAnnotated _ -> Core.TypeVariantAnnotated
  Core.TypeApplication _ -> Core.TypeVariantApplication
  Core.TypeElement _ -> Core.TypeVariantElement
  Core.TypeFunction _ -> Core.TypeVariantFunction
  Core.TypeLambda _ -> Core.TypeVariantLambda
  Core.TypeList _ -> Core.TypeVariantList
  Core.TypeLiteral _ -> Core.TypeVariantLiteral
  Core.TypeMap _ -> Core.TypeVariantMap
  Core.TypeNominal _ -> Core.TypeVariantNominal
  Core.TypeOptional _ -> Core.TypeVariantOptional
  Core.TypeRecord _ -> Core.TypeVariantRecord
  Core.TypeSet _ -> Core.TypeVariantSet
  Core.TypeUnion _ -> Core.TypeVariantUnion
  Core.TypeVariable _ -> Core.TypeVariantVariable) typ)

-- All type variants, in a canonical order
typeVariants :: [Core.TypeVariant]
typeVariants = [
  Core.TypeVariantAnnotated,
  Core.TypeVariantApplication,
  Core.TypeVariantElement,
  Core.TypeVariantFunction,
  Core.TypeVariantLambda,
  Core.TypeVariantList,
  Core.TypeVariantLiteral,
  Core.TypeVariantMap,
  Core.TypeVariantNominal,
  Core.TypeVariantOptional,
  Core.TypeVariantRecord,
  Core.TypeVariantSet,
  Core.TypeVariantUnion,
  Core.TypeVariantVariable]