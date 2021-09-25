module Hydra.Prototyping.Basics (
    atomicTypeVariant,
    atomicValueType,
    atomicValueVariant,
    floatTypeVariant,
    floatValueType,
    floatValueVariant,
    freeVariables,
    integerTypeVariant,
    integerValueType,
    integerValueVariant,
    termIsClosed,
    termIsValue,
    termVariant,
    typeVariant,
  ) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Prototyping.Helpers

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.Maybe as Y


atomicTypeVariant :: AtomicType -> AtomicVariant
atomicTypeVariant at = case at of
  AtomicTypeBinary -> AtomicVariantBinary
  AtomicTypeBoolean -> AtomicVariantBoolean
  AtomicTypeFloat _ -> AtomicVariantFloat
  AtomicTypeInteger _ -> AtomicVariantInteger
  AtomicTypeString -> AtomicVariantString

atomicValueType :: AtomicValue -> AtomicType
atomicValueType v = case v of
  AtomicValueBinary _ -> AtomicTypeBinary
  AtomicValueBoolean _ -> AtomicTypeBoolean
  AtomicValueFloat fv -> AtomicTypeFloat $ floatValueType fv
  AtomicValueInteger iv -> AtomicTypeInteger $ integerValueType iv
  AtomicValueString _ -> AtomicTypeString

atomicValueVariant :: AtomicValue -> AtomicVariant
atomicValueVariant = atomicTypeVariant . atomicValueType

floatTypeVariant :: FloatType -> FloatVariant
floatTypeVariant ft = case ft of
  FloatTypeBigfloat -> FloatVariantBigfloat
  FloatTypeFloat32 -> FloatVariantFloat32
  FloatTypeFloat64 -> FloatVariantFloat64

floatValueType :: FloatValue -> FloatType
floatValueType fv = case fv of
  FloatValueBigfloat _ -> FloatTypeBigfloat
  FloatValueFloat32 _ -> FloatTypeFloat32
  FloatValueFloat64 _ -> FloatTypeFloat64

floatValueVariant :: FloatValue -> FloatVariant
floatValueVariant = floatTypeVariant . floatValueType

freeVariables :: Term -> S.Set Variable
freeVariables term = S.fromList $ free S.empty term
  where
    free bound term = case term of
      TermApplication (Application t1 t2) -> free bound t1 ++ free bound t2
      TermAtomic _ -> []
      TermCases cases -> L.concatMap (free bound . fieldTerm) cases
      TermCompareTo term -> free bound term
      TermData -> []
      TermElement _ -> []
      TermFunction _ -> []
      TermLambda (Lambda v t) -> free (S.insert v bound) t
      TermList terms -> L.concatMap (free bound) terms
      TermProjection _ -> []
      TermRecord fields -> L.concatMap (free bound . fieldTerm) fields
      TermUnion field -> free bound $ fieldTerm field
      TermVariable v -> if S.member v bound then [] else [v]

integerTypeVariant :: IntegerType -> IntegerVariant
integerTypeVariant it = case it of
  IntegerTypeBigint -> IntegerVariantBigint
  IntegerTypeInt8 -> IntegerVariantInt8
  IntegerTypeInt16 -> IntegerVariantInt16
  IntegerTypeInt32 -> IntegerVariantInt32
  IntegerTypeInt64 -> IntegerVariantInt64
  IntegerTypeUint8 -> IntegerVariantUint8
  IntegerTypeUint16 -> IntegerVariantUint16
  IntegerTypeUint32 -> IntegerVariantUint32
  IntegerTypeUint64 -> IntegerVariantUint64

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

integerValueVariant :: IntegerValue -> IntegerVariant
integerValueVariant = integerTypeVariant . integerValueType

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: Term -> Bool
termIsClosed = S.null . freeVariables

-- | Whether a term has been fully reduced to a "value"
termIsValue :: EvaluationStrategy -> Term -> Bool
termIsValue strategy term = if isOpaque
    then True
    else case term of
      TermApplication _ -> False
      TermAtomic _ -> True
      TermCases cases -> checkFields cases
      TermCompareTo other -> termIsValue strategy other
      TermData -> True
      TermElement _ -> True
      TermFunction _ -> True
      TermLambda (Lambda _ body) -> termIsValue strategy body
      TermList els -> L.foldl (\b t -> b && termIsValue strategy t) True els
      TermProjection _ -> True
      TermRecord fields -> checkFields fields
      TermUnion field -> checkField field
      TermVariable _ -> False
  where
    isOpaque = S.member (termVariant term) (evaluationStrategyOpaqueTermVariants strategy)
    checkField = termIsValue strategy . fieldTerm
    checkFields = L.foldl (\b f -> b && checkField f) True

termVariant :: Term -> TermVariant
termVariant term = case term of
  TermApplication _ -> TermVariantApplication
  TermAtomic _ -> TermVariantAtomic
  TermCases _ -> TermVariantCases
  TermCompareTo _ -> TermVariantCompareTo
  TermData -> TermVariantData
  TermElement _ -> TermVariantElement
  TermFunction _ -> TermVariantFunction
  TermLambda _ -> TermVariantLambda
  TermList _ -> TermVariantList
  TermProjection _ -> TermVariantProjection
  TermRecord _ -> TermVariantRecord
  TermUnion _ -> TermVariantUnion
  TermVariable _ -> TermVariantVariable

typeVariant :: Type -> TypeVariant
typeVariant typ = case typ of
  TypeAtomic _ -> TypeVariantAtomic
  TypeElement _ -> TypeVariantElement
  TypeFunction _ -> TypeVariantFunction
  TypeList _ -> TypeVariantList
  TypeNominal _ -> TypeVariantNominal
  TypeRecord _ -> TypeVariantRecord
  TypeUnion _ -> TypeVariantUnion
