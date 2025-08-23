-- | Functions for working with term, type, and literal type variants, as well as numeric precision.

module Hydra.Variants where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Mantle as Mantle
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Find the elimination variant (constructor) for a given elimination term
eliminationVariant :: (Core.Elimination -> Mantle.EliminationVariant)
eliminationVariant x = case x of
  Core.EliminationProduct _ -> Mantle.EliminationVariantProduct
  Core.EliminationRecord _ -> Mantle.EliminationVariantRecord
  Core.EliminationUnion _ -> Mantle.EliminationVariantUnion
  Core.EliminationWrap _ -> Mantle.EliminationVariantWrap

-- | All elimination variants (constructors), in a canonical order
eliminationVariants :: [Mantle.EliminationVariant]
eliminationVariants = [
  Mantle.EliminationVariantProduct,
  Mantle.EliminationVariantRecord,
  Mantle.EliminationVariantUnion,
  Mantle.EliminationVariantWrap]

-- | Find the precision of a given floating-point type
floatTypePrecision :: (Core.FloatType -> Mantle.Precision)
floatTypePrecision x = case x of
  Core.FloatTypeBigfloat -> Mantle.PrecisionArbitrary
  Core.FloatTypeFloat32 -> (Mantle.PrecisionBits 32)
  Core.FloatTypeFloat64 -> (Mantle.PrecisionBits 64)

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

-- | Find the function variant (constructor) for a given function
functionVariant :: (Core.Function -> Mantle.FunctionVariant)
functionVariant x = case x of
  Core.FunctionElimination _ -> Mantle.FunctionVariantElimination
  Core.FunctionLambda _ -> Mantle.FunctionVariantLambda
  Core.FunctionPrimitive _ -> Mantle.FunctionVariantPrimitive

-- | All function variants (constructors), in a canonical order
functionVariants :: [Mantle.FunctionVariant]
functionVariants = [
  Mantle.FunctionVariantElimination,
  Mantle.FunctionVariantLambda,
  Mantle.FunctionVariantPrimitive]

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
integerTypePrecision :: (Core.IntegerType -> Mantle.Precision)
integerTypePrecision x = case x of
  Core.IntegerTypeBigint -> Mantle.PrecisionArbitrary
  Core.IntegerTypeInt8 -> (Mantle.PrecisionBits 8)
  Core.IntegerTypeInt16 -> (Mantle.PrecisionBits 16)
  Core.IntegerTypeInt32 -> (Mantle.PrecisionBits 32)
  Core.IntegerTypeInt64 -> (Mantle.PrecisionBits 64)
  Core.IntegerTypeUint8 -> (Mantle.PrecisionBits 8)
  Core.IntegerTypeUint16 -> (Mantle.PrecisionBits 16)
  Core.IntegerTypeUint32 -> (Mantle.PrecisionBits 32)
  Core.IntegerTypeUint64 -> (Mantle.PrecisionBits 64)

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

-- | Find the literal type variant (constructor) for a given literal value
literalTypeVariant :: (Core.LiteralType -> Mantle.LiteralVariant)
literalTypeVariant x = case x of
  Core.LiteralTypeBinary -> Mantle.LiteralVariantBinary
  Core.LiteralTypeBoolean -> Mantle.LiteralVariantBoolean
  Core.LiteralTypeFloat _ -> Mantle.LiteralVariantFloat
  Core.LiteralTypeInteger _ -> Mantle.LiteralVariantInteger
  Core.LiteralTypeString -> Mantle.LiteralVariantString

-- | All literal types, in a canonical order
literalTypes :: [Core.LiteralType]
literalTypes = (Lists.concat [
  [
    Core.LiteralTypeBinary,
    Core.LiteralTypeBoolean],
  Lists.map (\x -> Core.LiteralTypeFloat x) floatTypes,
  Lists.map (\x -> Core.LiteralTypeInteger x) integerTypes,
  [
    Core.LiteralTypeString]])

-- | Find the literal variant (constructor) for a given literal value
literalVariant :: (Core.Literal -> Mantle.LiteralVariant)
literalVariant arg_ = (literalTypeVariant (literalType arg_))

-- | All literal variants, in a canonical order
literalVariants :: [Mantle.LiteralVariant]
literalVariants = [
  Mantle.LiteralVariantBinary,
  Mantle.LiteralVariantBoolean,
  Mantle.LiteralVariantFloat,
  Mantle.LiteralVariantInteger,
  Mantle.LiteralVariantString]

-- | Find the term variant (constructor) for a given term
termVariant :: (Core.Term -> Mantle.TermVariant)
termVariant x = case x of
  Core.TermAnnotated _ -> Mantle.TermVariantAnnotated
  Core.TermApplication _ -> Mantle.TermVariantApplication
  Core.TermFunction _ -> Mantle.TermVariantFunction
  Core.TermLet _ -> Mantle.TermVariantLet
  Core.TermList _ -> Mantle.TermVariantList
  Core.TermLiteral _ -> Mantle.TermVariantLiteral
  Core.TermMap _ -> Mantle.TermVariantMap
  Core.TermOptional _ -> Mantle.TermVariantOptional
  Core.TermProduct _ -> Mantle.TermVariantProduct
  Core.TermRecord _ -> Mantle.TermVariantRecord
  Core.TermSet _ -> Mantle.TermVariantSet
  Core.TermSum _ -> Mantle.TermVariantSum
  Core.TermTypeLambda _ -> Mantle.TermVariantTypeLambda
  Core.TermTypeApplication _ -> Mantle.TermVariantTypeApplication
  Core.TermUnion _ -> Mantle.TermVariantUnion
  Core.TermUnit -> Mantle.TermVariantUnit
  Core.TermVariable _ -> Mantle.TermVariantVariable
  Core.TermWrap _ -> Mantle.TermVariantWrap

-- | All term (expression) variants, in a canonical order
termVariants :: [Mantle.TermVariant]
termVariants = [
  Mantle.TermVariantAnnotated,
  Mantle.TermVariantApplication,
  Mantle.TermVariantLiteral,
  Mantle.TermVariantFunction,
  Mantle.TermVariantList,
  Mantle.TermVariantMap,
  Mantle.TermVariantOptional,
  Mantle.TermVariantProduct,
  Mantle.TermVariantRecord,
  Mantle.TermVariantSet,
  Mantle.TermVariantSum,
  Mantle.TermVariantTypeLambda,
  Mantle.TermVariantTypeApplication,
  Mantle.TermVariantUnion,
  Mantle.TermVariantUnit,
  Mantle.TermVariantVariable,
  Mantle.TermVariantWrap]

-- | Find the type variant (constructor) for a given type
typeVariant :: (Core.Type -> Mantle.TypeVariant)
typeVariant x = case x of
  Core.TypeAnnotated _ -> Mantle.TypeVariantAnnotated
  Core.TypeApplication _ -> Mantle.TypeVariantApplication
  Core.TypeFunction _ -> Mantle.TypeVariantFunction
  Core.TypeForall _ -> Mantle.TypeVariantForall
  Core.TypeList _ -> Mantle.TypeVariantList
  Core.TypeLiteral _ -> Mantle.TypeVariantLiteral
  Core.TypeMap _ -> Mantle.TypeVariantMap
  Core.TypeOptional _ -> Mantle.TypeVariantOptional
  Core.TypeProduct _ -> Mantle.TypeVariantProduct
  Core.TypeRecord _ -> Mantle.TypeVariantRecord
  Core.TypeSet _ -> Mantle.TypeVariantSet
  Core.TypeSum _ -> Mantle.TypeVariantSum
  Core.TypeUnion _ -> Mantle.TypeVariantUnion
  Core.TypeUnit -> Mantle.TypeVariantUnit
  Core.TypeVariable _ -> Mantle.TypeVariantVariable
  Core.TypeWrap _ -> Mantle.TypeVariantWrap

-- | All type variants, in a canonical order
typeVariants :: [Mantle.TypeVariant]
typeVariants = [
  Mantle.TypeVariantAnnotated,
  Mantle.TypeVariantApplication,
  Mantle.TypeVariantFunction,
  Mantle.TypeVariantForall,
  Mantle.TypeVariantList,
  Mantle.TypeVariantLiteral,
  Mantle.TypeVariantMap,
  Mantle.TypeVariantWrap,
  Mantle.TypeVariantOptional,
  Mantle.TypeVariantProduct,
  Mantle.TypeVariantRecord,
  Mantle.TypeVariantSet,
  Mantle.TypeVariantSum,
  Mantle.TypeVariantUnion,
  Mantle.TypeVariantUnit,
  Mantle.TypeVariantVariable]
