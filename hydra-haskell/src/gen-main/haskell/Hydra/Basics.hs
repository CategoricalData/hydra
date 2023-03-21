-- | Basic functions for working with types and terms

module Hydra.Basics where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import Data.List
import Data.Map
import Data.Set

-- | Find the elimination variant (constructor) for a given elimination term
eliminationVariant :: (Core.Elimination a -> Mantle.EliminationVariant)
eliminationVariant x = case x of
  Core.EliminationElement -> Mantle.EliminationVariantElement
  Core.EliminationList _ -> Mantle.EliminationVariantList
  Core.EliminationOptional _ -> Mantle.EliminationVariantOptional
  Core.EliminationRecord _ -> Mantle.EliminationVariantRecord
  Core.EliminationUnion _ -> Mantle.EliminationVariantUnion
  Core.EliminationWrap _ -> Mantle.EliminationVariantWrap

-- | All elimination variants (constructors), in a canonical order
eliminationVariants :: [Mantle.EliminationVariant]
eliminationVariants = [
  Mantle.EliminationVariantElement,
  Mantle.EliminationVariantList,
  Mantle.EliminationVariantWrap,
  Mantle.EliminationVariantOptional,
  Mantle.EliminationVariantRecord,
  Mantle.EliminationVariantUnion]

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

functionArity :: (Core.Function a -> Int)
functionArity x = case x of
  Core.FunctionElimination _ -> 1
  Core.FunctionLambda v -> (Math.add 1 (termArity (Core.lambdaBody v)))
  Core.FunctionPrimitive _ -> 42

-- | Find the function variant (constructor) for a given function
functionVariant :: (Core.Function a -> Mantle.FunctionVariant)
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
  Core.LiteralFloat v -> ((\x2 -> Core.LiteralTypeFloat x2) (floatValueType v))
  Core.LiteralInteger v -> ((\x2 -> Core.LiteralTypeInteger x2) (integerValueType v))
  Core.LiteralString _ -> Core.LiteralTypeString

-- | Find the literal type variant (constructor) for a given literal value
literalTypeVariant :: (Core.LiteralType -> Mantle.LiteralVariant)
literalTypeVariant x = case x of
  Core.LiteralTypeBinary -> Mantle.LiteralVariantBinary
  Core.LiteralTypeBoolean -> Mantle.LiteralVariantBoolean
  Core.LiteralTypeFloat _ -> Mantle.LiteralVariantFloat
  Core.LiteralTypeInteger _ -> Mantle.LiteralVariantInteger
  Core.LiteralTypeString -> Mantle.LiteralVariantString

-- | Find the literal variant (constructor) for a given literal value
literalVariant :: (Core.Literal -> Mantle.LiteralVariant)
literalVariant x = (literalTypeVariant (literalType x))

-- | All literal variants, in a canonical order
literalVariants :: [Mantle.LiteralVariant]
literalVariants = [
  Mantle.LiteralVariantBinary,
  Mantle.LiteralVariantBoolean,
  Mantle.LiteralVariantFloat,
  Mantle.LiteralVariantInteger,
  Mantle.LiteralVariantString]

lookupPrimitive :: (Graph.Graph a -> Core.Name -> Maybe (Graph.Primitive a))
lookupPrimitive g name = (Maps.lookup name (Graph.graphPrimitives g))

-- | Find the arity (expected number of arguments) of a primitive constant or function
primitiveArity :: (Graph.Primitive a -> Int)
primitiveArity x = (typeArity (Graph.primitiveType x))

-- | Construct a qualified (dot-separated) name
qname :: (Module.Namespace -> String -> Core.Name)
qname ns name = (Core.Name (Strings.cat [
  Module.unNamespace ns,
  ".",
  name]))

termArity :: (Core.Term a -> Int)
termArity x = case x of
  Core.TermApplication v -> ((\x -> Math.sub x 1) (termArity (Core.applicationFunction v)))
  Core.TermFunction v -> (functionArity v)
  _ -> 0

-- | Find the term variant (constructor) for a given term
termVariant :: (Core.Term a -> Mantle.TermVariant)
termVariant term = ((\x -> case x of
  Core.TermAnnotated _ -> Mantle.TermVariantAnnotated
  Core.TermApplication _ -> Mantle.TermVariantApplication
  Core.TermElement _ -> Mantle.TermVariantElement
  Core.TermFunction _ -> Mantle.TermVariantFunction
  Core.TermLet _ -> Mantle.TermVariantLet
  Core.TermList _ -> Mantle.TermVariantList
  Core.TermLiteral _ -> Mantle.TermVariantLiteral
  Core.TermMap _ -> Mantle.TermVariantMap
  Core.TermOptional _ -> Mantle.TermVariantOptional
  Core.TermProduct _ -> Mantle.TermVariantProduct
  Core.TermRecord _ -> Mantle.TermVariantRecord
  Core.TermSet _ -> Mantle.TermVariantSet
  Core.TermStream _ -> Mantle.TermVariantStream
  Core.TermSum _ -> Mantle.TermVariantSum
  Core.TermUnion _ -> Mantle.TermVariantUnion
  Core.TermVariable _ -> Mantle.TermVariantVariable
  Core.TermWrap _ -> Mantle.TermVariantWrap) term)

-- | All term (expression) variants, in a canonical order
termVariants :: [Mantle.TermVariant]
termVariants = [
  Mantle.TermVariantAnnotated,
  Mantle.TermVariantApplication,
  Mantle.TermVariantLiteral,
  Mantle.TermVariantElement,
  Mantle.TermVariantFunction,
  Mantle.TermVariantList,
  Mantle.TermVariantMap,
  Mantle.TermVariantOptional,
  Mantle.TermVariantProduct,
  Mantle.TermVariantRecord,
  Mantle.TermVariantSet,
  Mantle.TermVariantStream,
  Mantle.TermVariantSum,
  Mantle.TermVariantUnion,
  Mantle.TermVariantVariable,
  Mantle.TermVariantWrap]

-- | TODO: temporary. Just a token polymorphic function for testing
testLists :: ([[a]] -> Int)
testLists els = (Lists.length (Lists.concat els))

typeArity :: (Core.Type a -> Int)
typeArity x = case x of
  Core.TypeAnnotated v -> (typeArity (Core.annotatedSubject v))
  Core.TypeApplication v -> (typeArity (Core.applicationTypeFunction v))
  Core.TypeLambda v -> (typeArity (Core.lambdaTypeBody v))
  Core.TypeFunction v -> (Math.add 1 (typeArity (Core.functionTypeCodomain v)))
  _ -> 0

-- | Find the type variant (constructor) for a given type
typeVariant :: (Core.Type a -> Mantle.TypeVariant)
typeVariant typ = ((\x -> case x of
  Core.TypeAnnotated _ -> Mantle.TypeVariantAnnotated
  Core.TypeApplication _ -> Mantle.TypeVariantApplication
  Core.TypeElement _ -> Mantle.TypeVariantElement
  Core.TypeFunction _ -> Mantle.TypeVariantFunction
  Core.TypeLambda _ -> Mantle.TypeVariantLambda
  Core.TypeList _ -> Mantle.TypeVariantList
  Core.TypeLiteral _ -> Mantle.TypeVariantLiteral
  Core.TypeMap _ -> Mantle.TypeVariantMap
  Core.TypeOptional _ -> Mantle.TypeVariantOptional
  Core.TypeProduct _ -> Mantle.TypeVariantProduct
  Core.TypeRecord _ -> Mantle.TypeVariantRecord
  Core.TypeSet _ -> Mantle.TypeVariantSet
  Core.TypeStream _ -> Mantle.TypeVariantStream
  Core.TypeSum _ -> Mantle.TypeVariantSum
  Core.TypeUnion _ -> Mantle.TypeVariantUnion
  Core.TypeVariable _ -> Mantle.TypeVariantVariable
  Core.TypeWrap _ -> Mantle.TypeVariantWrap) typ)

-- | All type variants, in a canonical order
typeVariants :: [Mantle.TypeVariant]
typeVariants = [
  Mantle.TypeVariantAnnotated,
  Mantle.TypeVariantApplication,
  Mantle.TypeVariantElement,
  Mantle.TypeVariantFunction,
  Mantle.TypeVariantLambda,
  Mantle.TypeVariantList,
  Mantle.TypeVariantLiteral,
  Mantle.TypeVariantMap,
  Mantle.TypeVariantWrap,
  Mantle.TypeVariantOptional,
  Mantle.TypeVariantProduct,
  Mantle.TypeVariantRecord,
  Mantle.TypeVariantSet,
  Mantle.TypeVariantStream,
  Mantle.TypeVariantSum,
  Mantle.TypeVariantUnion,
  Mantle.TypeVariantVariable]