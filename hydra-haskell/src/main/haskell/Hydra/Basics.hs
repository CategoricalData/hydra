module Hydra.Basics where

import Hydra.Core

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
  FunctionPrimitive _ -> FunctionVariantPrimitive
  FunctionProjection _ -> FunctionVariantProjection

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

-- Find the literal type variant (constructor) for a given literal value
literalTypeVariant :: (LiteralType -> LiteralVariant)
literalTypeVariant x = case x of
  LiteralTypeBinary -> LiteralVariantBinary
  LiteralTypeBoolean -> LiteralVariantBoolean
  LiteralTypeFloat _ -> LiteralVariantFloat
  LiteralTypeInteger _ -> LiteralVariantInteger
  LiteralTypeString -> LiteralVariantString

-- Find the literal type for a given literal value
literalType :: (Literal -> LiteralType)
literalType x = case x of
  LiteralBinary _ -> LiteralTypeBinary
  LiteralBoolean _ -> LiteralTypeBoolean
  LiteralFloat v -> (LiteralTypeFloat (floatValueType v))
  LiteralInteger v -> (LiteralTypeInteger (integerValueType v))
  LiteralString _ -> LiteralTypeString

-- Find the literal variant (constructor) for a given literal value
literalVariant :: (Literal -> LiteralVariant)
literalVariant x = (literalTypeVariant (literalType x))

-- Find the term variant (constructor) for a given term
termVariant :: (Term a -> TermVariant)
termVariant term = (
  (
    \x -> case x of
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