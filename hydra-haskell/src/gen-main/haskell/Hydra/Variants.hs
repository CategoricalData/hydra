-- | Functions for working with term, type, and literal type variants, as well as numeric precision.

module Hydra.Variants where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Meta as Meta
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Find the elimination variant (constructor) for a given elimination term
eliminationVariant :: (Core.Elimination -> Meta.EliminationVariant)
eliminationVariant x = case x of
  Core.EliminationProduct _ -> Meta.EliminationVariantProduct
  Core.EliminationRecord _ -> Meta.EliminationVariantRecord
  Core.EliminationUnion _ -> Meta.EliminationVariantUnion
  Core.EliminationWrap _ -> Meta.EliminationVariantWrap

-- | All elimination variants (constructors), in a canonical order
eliminationVariants :: [Meta.EliminationVariant]
eliminationVariants = [
  Meta.EliminationVariantProduct,
  Meta.EliminationVariantRecord,
  Meta.EliminationVariantUnion,
  Meta.EliminationVariantWrap]

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

-- | Find the function variant (constructor) for a given function
functionVariant :: (Core.Function -> Meta.FunctionVariant)
functionVariant x = case x of
  Core.FunctionElimination _ -> Meta.FunctionVariantElimination
  Core.FunctionLambda _ -> Meta.FunctionVariantLambda
  Core.FunctionPrimitive _ -> Meta.FunctionVariantPrimitive

-- | All function variants (constructors), in a canonical order
functionVariants :: [Meta.FunctionVariant]
functionVariants = [
  Meta.FunctionVariantElimination,
  Meta.FunctionVariantLambda,
  Meta.FunctionVariantPrimitive]

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

-- | Find the literal type variant (constructor) for a given literal value
literalTypeVariant :: (Core.LiteralType -> Meta.LiteralVariant)
literalTypeVariant x = case x of
  Core.LiteralTypeBinary -> Meta.LiteralVariantBinary
  Core.LiteralTypeBoolean -> Meta.LiteralVariantBoolean
  Core.LiteralTypeFloat _ -> Meta.LiteralVariantFloat
  Core.LiteralTypeInteger _ -> Meta.LiteralVariantInteger
  Core.LiteralTypeString -> Meta.LiteralVariantString

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
literalVariant :: (Core.Literal -> Meta.LiteralVariant)
literalVariant arg_ = (literalTypeVariant (literalType arg_))

-- | All literal variants, in a canonical order
literalVariants :: [Meta.LiteralVariant]
literalVariants = [
  Meta.LiteralVariantBinary,
  Meta.LiteralVariantBoolean,
  Meta.LiteralVariantFloat,
  Meta.LiteralVariantInteger,
  Meta.LiteralVariantString]

-- | Find the term variant (constructor) for a given term
termVariant :: (Core.Term -> Meta.TermVariant)
termVariant x = case x of
  Core.TermAnnotated _ -> Meta.TermVariantAnnotated
  Core.TermApplication _ -> Meta.TermVariantApplication
  Core.TermEither _ -> Meta.TermVariantEither
  Core.TermFunction _ -> Meta.TermVariantFunction
  Core.TermLet _ -> Meta.TermVariantLet
  Core.TermList _ -> Meta.TermVariantList
  Core.TermLiteral _ -> Meta.TermVariantLiteral
  Core.TermMap _ -> Meta.TermVariantMap
  Core.TermMaybe _ -> Meta.TermVariantMaybe
  Core.TermPair _ -> Meta.TermVariantPair
  Core.TermProduct _ -> Meta.TermVariantProduct
  Core.TermRecord _ -> Meta.TermVariantRecord
  Core.TermSet _ -> Meta.TermVariantSet
  Core.TermSum _ -> Meta.TermVariantSum
  Core.TermTypeApplication _ -> Meta.TermVariantTypeApplication
  Core.TermTypeLambda _ -> Meta.TermVariantTypeLambda
  Core.TermUnion _ -> Meta.TermVariantUnion
  Core.TermUnit -> Meta.TermVariantUnit
  Core.TermVariable _ -> Meta.TermVariantVariable
  Core.TermWrap _ -> Meta.TermVariantWrap

-- | All term (expression) variants, in a canonical order
termVariants :: [Meta.TermVariant]
termVariants = [
  Meta.TermVariantAnnotated,
  Meta.TermVariantApplication,
  Meta.TermVariantEither,
  Meta.TermVariantFunction,
  Meta.TermVariantList,
  Meta.TermVariantLiteral,
  Meta.TermVariantMap,
  Meta.TermVariantMaybe,
  Meta.TermVariantPair,
  Meta.TermVariantProduct,
  Meta.TermVariantRecord,
  Meta.TermVariantSet,
  Meta.TermVariantSum,
  Meta.TermVariantTypeLambda,
  Meta.TermVariantTypeApplication,
  Meta.TermVariantUnion,
  Meta.TermVariantUnit,
  Meta.TermVariantVariable,
  Meta.TermVariantWrap]

-- | Find the type variant (constructor) for a given type
typeVariant :: (Core.Type -> Meta.TypeVariant)
typeVariant x = case x of
  Core.TypeAnnotated _ -> Meta.TypeVariantAnnotated
  Core.TypeApplication _ -> Meta.TypeVariantApplication
  Core.TypeEither _ -> Meta.TypeVariantEither
  Core.TypeFunction _ -> Meta.TypeVariantFunction
  Core.TypeForall _ -> Meta.TypeVariantForall
  Core.TypeList _ -> Meta.TypeVariantList
  Core.TypeLiteral _ -> Meta.TypeVariantLiteral
  Core.TypeMap _ -> Meta.TypeVariantMap
  Core.TypeMaybe _ -> Meta.TypeVariantMaybe
  Core.TypePair _ -> Meta.TypeVariantPair
  Core.TypeProduct _ -> Meta.TypeVariantProduct
  Core.TypeRecord _ -> Meta.TypeVariantRecord
  Core.TypeSet _ -> Meta.TypeVariantSet
  Core.TypeSum _ -> Meta.TypeVariantSum
  Core.TypeUnion _ -> Meta.TypeVariantUnion
  Core.TypeUnit -> Meta.TypeVariantUnit
  Core.TypeVariable _ -> Meta.TypeVariantVariable
  Core.TypeWrap _ -> Meta.TypeVariantWrap

-- | All type variants, in a canonical order
typeVariants :: [Meta.TypeVariant]
typeVariants = [
  Meta.TypeVariantAnnotated,
  Meta.TypeVariantApplication,
  Meta.TypeVariantEither,
  Meta.TypeVariantFunction,
  Meta.TypeVariantForall,
  Meta.TypeVariantList,
  Meta.TypeVariantLiteral,
  Meta.TypeVariantMap,
  Meta.TypeVariantWrap,
  Meta.TypeVariantMaybe,
  Meta.TypeVariantPair,
  Meta.TypeVariantProduct,
  Meta.TypeVariantRecord,
  Meta.TypeVariantSet,
  Meta.TypeVariantSum,
  Meta.TypeVariantUnion,
  Meta.TypeVariantUnit,
  Meta.TypeVariantVariable]
