module Hydra.Prototyping.Basics (
    atomicTypeAsTerm,
    atomicTypeVariant,
    atomicValueType,
    atomicValueVariant,
    fieldTypeAsTerm,
    floatTypeAsTerm,
    floatTypeVariant,
    floatValueType,
    floatValueVariant,
    freeVariables,
    functionTypeAsTerm,
    integerTypeAsTerm,
    integerTypeVariant,
    integerValueType,
    integerValueVariant,
    termIsClosed,
    termVariant,
    typeAsTerm,
    typeVariant,
    unitTerm,
  ) where

import Hydra.Core
--import Hydra.Graph
--import Hydra.Evaluation

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S


atomicTypeAsTerm :: AtomicType -> Term
atomicTypeAsTerm at = case at of
  AtomicTypeBinary -> unitVariant "binary"
  AtomicTypeBoolean -> unitVariant "boolean"
  AtomicTypeFloat ft -> variant "float" $ floatTypeAsTerm ft
  AtomicTypeInteger it -> variant "integer" $ integerTypeAsTerm it
  AtomicTypeString -> unitVariant "string"

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
atomicValueVariant av = case av of
  AtomicValueBinary _ -> AtomicVariantBinary
  AtomicValueBoolean _ -> AtomicVariantBoolean
  AtomicValueFloat _ -> AtomicVariantFloat
  AtomicValueInteger _ -> AtomicVariantInteger
  AtomicValueString _ -> AtomicVariantString

fieldTypeAsTerm :: FieldType -> Term
fieldTypeAsTerm (FieldType fname t) = TermRecord [
  Field "name" $ string fname,
  Field "type" $ typeAsTerm t]

floatTypeAsTerm :: FloatType -> Term
floatTypeAsTerm ft = unitVariant $ case ft of
  FloatTypeBigfloat -> "bigfloat"
  FloatTypeFloat32 -> "float32"
  FloatTypeFloat64 -> "float64"

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
floatValueVariant fv = case fv of
  FloatValueBigfloat _ -> FloatVariantBigfloat
  FloatValueFloat32 _ -> FloatVariantFloat32
  FloatValueFloat64 _ -> FloatVariantFloat64

freeVariables :: Term -> S.Set Variable
freeVariables term = S.fromList $ free S.empty term
  where
    free bound term = case term of
      TermApplication (Application t1 t2) -> free bound t1 ++ free bound t2
      TermAtomic _ -> []
      TermCases (CaseStatement cases def) -> free bound def ++ L.concatMap (free bound . fieldTerm) cases
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

functionTypeAsTerm :: FunctionType -> Term
functionTypeAsTerm (FunctionType dom cod) = TermRecord [
  Field "domain" $ typeAsTerm dom,
  Field "codomain" $ typeAsTerm cod]

integerTypeAsTerm :: IntegerType -> Term
integerTypeAsTerm it = unitVariant $ case it of
  IntegerTypeBigint -> "bigint"
  IntegerTypeInt8 -> "int8"
  IntegerTypeInt16 -> "int16"
  IntegerTypeInt32 -> "int32"
  IntegerTypeInt64 -> "int64"
  IntegerTypeUint8 -> "uint8"
  IntegerTypeUint16 -> "uint16"
  IntegerTypeUint32 -> "uint32"
  IntegerTypeUint64 -> "uint64"

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
integerValueVariant iv = case iv of
  IntegerValueBigint _ -> IntegerVariantBigint
  IntegerValueInt8 _ -> IntegerVariantInt8
  IntegerValueInt16 _ -> IntegerVariantInt16
  IntegerValueInt32 _ -> IntegerVariantInt32
  IntegerValueInt64 _ -> IntegerVariantInt64
  IntegerValueUint8 _ -> IntegerVariantUint8
  IntegerValueUint16 _ -> IntegerVariantUint16
  IntegerValueUint32 _ -> IntegerVariantUint32
  IntegerValueUint64 _ -> IntegerVariantUint64

string = TermAtomic . AtomicValueString

-- | Whether a term is closed, i.e. represents a complete program
termIsClosed :: Term -> Bool
termIsClosed = S.null . freeVariables

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

typeAsTerm :: Type -> Term
typeAsTerm typ = case typ of
    TypeAtomic at -> variant "atomic" $ atomicTypeAsTerm at
    TypeElement t -> variant "element" $ typeAsTerm t
    TypeFunction ft -> variant "function" $ functionTypeAsTerm ft
    TypeList t -> variant "list" $ typeAsTerm t
    TypeNominal name -> variant "nominal" $ string name
    TypeRecord fields -> variant "record" $ TermList $ fmap fieldTypeAsTerm fields
    TypeUnion fields -> variant "union" $ TermList $ fmap fieldTypeAsTerm fields

typeVariant :: Type -> TypeVariant
typeVariant typ = case typ of
  TypeAtomic _ -> TypeVariantAtomic
  TypeElement _ -> TypeVariantElement
  TypeFunction _ -> TypeVariantFunction
  TypeList _ -> TypeVariantList
  TypeNominal _ -> TypeVariantNominal
  TypeRecord _ -> TypeVariantRecord
  TypeUnion _ -> TypeVariantUnion

unitTerm :: Term
unitTerm = TermRecord []

unitVariant fname = variant fname unitTerm

variant fname term = TermUnion (Field fname term)
