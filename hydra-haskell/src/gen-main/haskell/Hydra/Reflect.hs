-- Note: this is an automatically generated file. Do not edit.

-- | Reflection functions for working with term, type, and literal type variants, as well as numeric precision.

module Hydra.Reflect where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Util as Util
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Find the elimination inject (constructor) for a given elimination term
eliminationVariant :: (Core.Elimination -> Variants.EliminationVariant)
eliminationVariant x = case x of
  Core.EliminationRecord _ -> Variants.EliminationVariantRecord
  Core.EliminationUnion _ -> Variants.EliminationVariantUnion
  Core.EliminationWrap _ -> Variants.EliminationVariantWrap

-- | All elimination variants (constructors), in a canonical order
eliminationVariants :: [Variants.EliminationVariant]
eliminationVariants = [
  Variants.EliminationVariantRecord,
  Variants.EliminationVariantUnion,
  Variants.EliminationVariantWrap]

-- | Find the precision of a given floating-point type
floatTypePrecision :: (Core.FloatType -> Util.Precision)
floatTypePrecision x = case x of
  Core.FloatTypeBigfloat -> Util.PrecisionArbitrary
  Core.FloatTypeFloat32 -> (Util.PrecisionBits 32)
  Core.FloatTypeFloat64 -> (Util.PrecisionBits 64)

-- | All floating-point types in a canonical order
floatTypes :: [Core.FloatType]
floatTypes = [
  Core.FloatTypeBigfloat,
  Core.FloatTypeFloat32,
  Core.FloatTypeFloat64]

-- | Find the float type for a given floating-point value
floatValueType :: (Core.FloatValue -> Core.FloatType)
floatValueType x = case x of
  Core.FloatValueBigfloat _ -> Core.FloatTypeBigfloat
  Core.FloatValueFloat32 _ -> Core.FloatTypeFloat32
  Core.FloatValueFloat64 _ -> Core.FloatTypeFloat64

-- | Find the function inject (constructor) for a given function
functionVariant :: (Core.Function -> Variants.FunctionVariant)
functionVariant x = case x of
  Core.FunctionElimination _ -> Variants.FunctionVariantElimination
  Core.FunctionLambda _ -> Variants.FunctionVariantLambda
  Core.FunctionPrimitive _ -> Variants.FunctionVariantPrimitive

-- | All function variants (constructors), in a canonical order
functionVariants :: [Variants.FunctionVariant]
functionVariants = [
  Variants.FunctionVariantElimination,
  Variants.FunctionVariantLambda,
  Variants.FunctionVariantPrimitive]

-- | Find whether a given integer type is signed (true) or unsigned (false)
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

-- | Find the precision of a given integer type
integerTypePrecision :: (Core.IntegerType -> Util.Precision)
integerTypePrecision x = case x of
  Core.IntegerTypeBigint -> Util.PrecisionArbitrary
  Core.IntegerTypeInt8 -> (Util.PrecisionBits 8)
  Core.IntegerTypeInt16 -> (Util.PrecisionBits 16)
  Core.IntegerTypeInt32 -> (Util.PrecisionBits 32)
  Core.IntegerTypeInt64 -> (Util.PrecisionBits 64)
  Core.IntegerTypeUint8 -> (Util.PrecisionBits 8)
  Core.IntegerTypeUint16 -> (Util.PrecisionBits 16)
  Core.IntegerTypeUint32 -> (Util.PrecisionBits 32)
  Core.IntegerTypeUint64 -> (Util.PrecisionBits 64)

-- | All integer types, in a canonical order
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

-- | Find the integer type for a given integer value
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

-- | Find the literal type for a given literal value
literalType :: (Core.Literal -> Core.LiteralType)
literalType x = case x of
  Core.LiteralBinary _ -> Core.LiteralTypeBinary
  Core.LiteralBoolean _ -> Core.LiteralTypeBoolean
  Core.LiteralFloat v1 -> ((\injected_ -> Core.LiteralTypeFloat injected_) (floatValueType v1))
  Core.LiteralInteger v1 -> ((\injected_ -> Core.LiteralTypeInteger injected_) (integerValueType v1))
  Core.LiteralString _ -> Core.LiteralTypeString

-- | Find the literal type inject (constructor) for a given literal value
literalTypeVariant :: (Core.LiteralType -> Variants.LiteralVariant)
literalTypeVariant x = case x of
  Core.LiteralTypeBinary -> Variants.LiteralVariantBinary
  Core.LiteralTypeBoolean -> Variants.LiteralVariantBoolean
  Core.LiteralTypeFloat _ -> Variants.LiteralVariantFloat
  Core.LiteralTypeInteger _ -> Variants.LiteralVariantInteger
  Core.LiteralTypeString -> Variants.LiteralVariantString

-- | All literal types, in a canonical order
literalTypes :: [Core.LiteralType]
literalTypes = (Lists.concat [
  [
    Core.LiteralTypeBinary,
    Core.LiteralTypeBoolean],
  (Lists.map (\x -> Core.LiteralTypeFloat x) floatTypes),
  (Lists.map (\x -> Core.LiteralTypeInteger x) integerTypes),
  [
    Core.LiteralTypeString]])

-- | Find the literal inject (constructor) for a given literal value
literalVariant :: (Core.Literal -> Variants.LiteralVariant)
literalVariant arg_ = (literalTypeVariant (literalType arg_))

-- | All literal variants, in a canonical order
literalVariants :: [Variants.LiteralVariant]
literalVariants = [
  Variants.LiteralVariantBinary,
  Variants.LiteralVariantBoolean,
  Variants.LiteralVariantFloat,
  Variants.LiteralVariantInteger,
  Variants.LiteralVariantString]

-- | Find the term inject (constructor) for a given term
termVariant :: (Core.Term -> Variants.TermVariant)
termVariant x = case x of
  Core.TermAnnotated _ -> Variants.TermVariantAnnotated
  Core.TermApplication _ -> Variants.TermVariantApplication
  Core.TermEither _ -> Variants.TermVariantEither
  Core.TermFunction _ -> Variants.TermVariantFunction
  Core.TermLet _ -> Variants.TermVariantLet
  Core.TermList _ -> Variants.TermVariantList
  Core.TermLiteral _ -> Variants.TermVariantLiteral
  Core.TermMap _ -> Variants.TermVariantMap
  Core.TermMaybe _ -> Variants.TermVariantMaybe
  Core.TermPair _ -> Variants.TermVariantPair
  Core.TermRecord _ -> Variants.TermVariantRecord
  Core.TermSet _ -> Variants.TermVariantSet
  Core.TermTypeApplication _ -> Variants.TermVariantTypeApplication
  Core.TermTypeLambda _ -> Variants.TermVariantTypeLambda
  Core.TermUnion _ -> Variants.TermVariantUnion
  Core.TermUnit -> Variants.TermVariantUnit
  Core.TermVariable _ -> Variants.TermVariantVariable
  Core.TermWrap _ -> Variants.TermVariantWrap

-- | All term (expression) variants, in a canonical order
termVariants :: [Variants.TermVariant]
termVariants = [
  Variants.TermVariantAnnotated,
  Variants.TermVariantApplication,
  Variants.TermVariantEither,
  Variants.TermVariantFunction,
  Variants.TermVariantList,
  Variants.TermVariantLiteral,
  Variants.TermVariantMap,
  Variants.TermVariantMaybe,
  Variants.TermVariantPair,
  Variants.TermVariantRecord,
  Variants.TermVariantSet,
  Variants.TermVariantTypeLambda,
  Variants.TermVariantTypeApplication,
  Variants.TermVariantUnion,
  Variants.TermVariantUnit,
  Variants.TermVariantVariable,
  Variants.TermVariantWrap]

-- | Find the type inject (constructor) for a given type
typeVariant :: (Core.Type -> Variants.TypeVariant)
typeVariant x = case x of
  Core.TypeAnnotated _ -> Variants.TypeVariantAnnotated
  Core.TypeApplication _ -> Variants.TypeVariantApplication
  Core.TypeEither _ -> Variants.TypeVariantEither
  Core.TypeFunction _ -> Variants.TypeVariantFunction
  Core.TypeForall _ -> Variants.TypeVariantForall
  Core.TypeList _ -> Variants.TypeVariantList
  Core.TypeLiteral _ -> Variants.TypeVariantLiteral
  Core.TypeMap _ -> Variants.TypeVariantMap
  Core.TypeMaybe _ -> Variants.TypeVariantMaybe
  Core.TypePair _ -> Variants.TypeVariantPair
  Core.TypeRecord _ -> Variants.TypeVariantRecord
  Core.TypeSet _ -> Variants.TypeVariantSet
  Core.TypeUnion _ -> Variants.TypeVariantUnion
  Core.TypeUnit -> Variants.TypeVariantUnit
  Core.TypeVariable _ -> Variants.TypeVariantVariable
  Core.TypeWrap _ -> Variants.TypeVariantWrap

-- | All type variants, in a canonical order
typeVariants :: [Variants.TypeVariant]
typeVariants = [
  Variants.TypeVariantAnnotated,
  Variants.TypeVariantApplication,
  Variants.TypeVariantEither,
  Variants.TypeVariantFunction,
  Variants.TypeVariantForall,
  Variants.TypeVariantList,
  Variants.TypeVariantLiteral,
  Variants.TypeVariantMap,
  Variants.TypeVariantWrap,
  Variants.TypeVariantMaybe,
  Variants.TypeVariantPair,
  Variants.TypeVariantRecord,
  Variants.TypeVariantSet,
  Variants.TypeVariantUnion,
  Variants.TypeVariantUnit,
  Variants.TypeVariantVariable]
