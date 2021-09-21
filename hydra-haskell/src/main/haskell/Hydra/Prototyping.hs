module Hydra.Prototyping (
    freeVariables,
    termVariant,
    typeAsTerm,
    typeVariant,
    unitTerm,
  ) where

import Hydra.Core
import Hydra.Graph
--import Hydra.Evaluation

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S


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
    TypeRecord fields -> variant "record" $ TermList $ fmap fieldAsTerm fields
    TypeUnion fields -> variant "union" $ TermList $ fmap fieldAsTerm fields

  where
    atomicTypeAsTerm at = case at of
      AtomicTypeBinary -> unitVariant "binary"
      AtomicTypeBoolean -> unitVariant "boolean"
      AtomicTypeFloat ft -> variant "float" $ floatTypeAsTerm ft
      AtomicTypeInteger it -> variant "integer" $ integerTypeAsTerm it
      AtomicTypeString -> unitVariant "string"

    fieldAsTerm (FieldType fname t) = TermRecord [
      Field "name" $ string fname,
      Field "type" $ typeAsTerm t]

    floatTypeAsTerm ft = unitVariant $ case ft of
      FloatTypeBigfloat -> "bigfloat"
      FloatTypeFloat32 -> "float32"
      FloatTypeFloat64 -> "float64"

    functionTypeAsTerm (FunctionType dom cod) = TermRecord [
      Field "domain" $ typeAsTerm dom,
      Field "codomain" $ typeAsTerm cod]

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

    string = TermAtomic . AtomicValueString

    unitVariant fname = variant fname unitTerm

    variant fname term = TermUnion (Field fname term)

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
