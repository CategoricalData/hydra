module Hydra.CoreEncoding where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Terms

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


encodeApplication :: (Default m, Ord m) => Context m -> Application m -> Term m
encodeApplication cx (Application f a) = nominalRecord cx _Application [
  Field _Application_function $ encodeTerm cx f,
  Field _Application_argument $ encodeTerm cx f]

encodeElimination :: (Default m, Ord m) => Context m -> Elimination m -> Term m
encodeElimination cx e = case e of
  EliminationElement -> unitVariant _Elimination_element
  EliminationNominal (Name name) -> variant _Elimination_nominal $ string name
  EliminationOptional cases -> variant _Elimination_optional $ encodeOptionalCases cx cases
  EliminationRecord (FieldName fname) -> variant _Elimination_record $ string fname
  EliminationUnion cases -> variant _Elimination_union $ list $ encodeField cx <$> cases

encodeField :: (Default m, Ord m) => Context m -> Field m -> Term m
encodeField cx (Field (FieldName name) term) = nominalRecord cx _Field [
  Field _Field_name $ string name,
  Field _Field_term $ encodeTerm cx term]

encodeFieldType :: Default m => Context m -> FieldType m -> Term m
encodeFieldType cx (FieldType (FieldName fname) t) = nominalRecord cx _FieldType [
  Field _FieldType_name $ string fname,
  Field _FieldType_type $ encodeType cx t]

encodeFloatType :: Default m => Context m -> FloatType -> Term m
encodeFloatType cx ft = unitVariant $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

encodeFunction :: (Default m, Ord m) => Context m -> Function m -> Term m
encodeFunction cx f = case f of
  FunctionCompareTo other -> variant _Function_compareTo $ encodeTerm cx other
  FunctionElimination e -> variant _Function_compareTo $ encodeElimination cx e
  FunctionLambda l -> variant _Function_lambda $ encodeLambda cx l
  FunctionPrimitive (Name name) -> variant _Function_primitive $ string name

encodeFunctionType :: Default m => Context m -> FunctionType m -> Term m
encodeFunctionType cx (FunctionType dom cod) = nominalRecord cx _FunctionType [
  Field _FunctionType_domain $ encodeType cx dom,
  Field _FunctionType_codomain $ encodeType cx cod]

encodeIntegerType :: Default m => Context m -> IntegerType -> Term m
encodeIntegerType cx it = unitVariant $ case it of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

encodeLambda :: (Default m, Ord m) => Context m -> Lambda m -> Term m
encodeLambda cx (Lambda (Variable v) b) = nominalRecord cx _Lambda [
  Field _Lambda_parameter $ string v,
  Field _Lambda_body $ encodeTerm cx b]

encodeLiteralType :: Default m => Context m -> LiteralType -> Term m
encodeLiteralType cx at = case at of
  LiteralTypeBinary -> unitVariant _LiteralType_binary
  LiteralTypeBoolean -> unitVariant _LiteralType_boolean
  LiteralTypeFloat ft -> variant _LiteralType_float $ encodeFloatType cx ft
  LiteralTypeInteger it -> variant _LiteralType_integer $ encodeIntegerType cx it
  LiteralTypeString -> unitVariant _LiteralType_string

encodeLiteral :: Default m => Context m -> Literal -> Term m
encodeLiteral _ = literal

encodeLiteralVariant :: Default m => Context m -> LiteralVariant -> Term m
encodeLiteralVariant _ av = unitVariant $ case av of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

encodeMapType :: Default m => Context m -> MapType m -> Term m
encodeMapType cx (MapType kt vt) = nominalRecord cx _MapType [
  Field _MapType_keys $ encodeType cx kt,
  Field _MapType_values $ encodeType cx vt]

encodeNamed :: (Default m, Ord m) => Context m -> Named m -> Term m
encodeNamed cx (Named (Name name) term) = nominalRecord cx _Named [
  Field _Named_typeName $ string name,
  Field _Named_term $ encodeTerm cx term]

encodeOptionalCases :: (Default m, Ord m) => Context m -> OptionalCases m -> Term m
encodeOptionalCases cx (OptionalCases nothing just) = nominalRecord cx _OptionalCases [
  Field _OptionalCases_nothing $ encodeTerm cx nothing,
  Field _OptionalCases_just $ encodeTerm cx just]

encodeTerm :: (Default m, Ord m) => Context m -> Term m -> Term m
encodeTerm cx term = case termExpr term of
  TermExprApplication a -> variant _TermExpr_application $ encodeApplication cx a
  TermExprLiteral av -> variant _TermExpr_literal $ encodeLiteral cx av
  TermExprElement (Name name) -> variant _TermExpr_element $ string name
  TermExprFunction f -> variant _TermExpr_function $ encodeFunction cx f
  TermExprList terms -> variant _TermExpr_list $ list $ encodeTerm cx <$> terms
  TermExprMap m -> variant _TermExpr_map $ map $ M.fromList $ encodePair <$> M.toList m
    where encodePair (k, v) = (encodeTerm cx k, encodeTerm cx v)
  TermExprNominal ntt -> variant _TermExpr_nominal $ encodeNamed cx ntt
  TermExprOptional m -> variant _TermExpr_optional $ optional $ encodeTerm cx <$> m
  TermExprRecord fields -> variant _TermExpr_record $ list $ encodeField cx <$> fields
  TermExprSet terms -> variant _TermExpr_set $ set $ S.fromList $ encodeTerm cx <$> S.toList terms
  TermExprUnion field -> variant _TermExpr_union $ encodeField cx field
  TermExprVariable (Variable var) -> variant _TermExpr_variable $ string var

encodeType :: Default m => Context m -> Type m -> Term m
encodeType cx typ = setMeta (typeMeta typ) $ case typeExpr typ of
  TypeExprLiteral at -> variant _TypeExpr_literal $ encodeLiteralType cx at
  TypeExprElement t -> variant _TypeExpr_element $ encodeType cx t
  TypeExprFunction ft -> variant _TypeExpr_function $ encodeFunctionType cx ft
  TypeExprList t -> variant _TypeExpr_list $ encodeType cx t
  TypeExprMap mt -> variant _TypeExpr_map $ encodeMapType cx mt
  TypeExprNominal name -> variant _TypeExpr_nominal $ element name
  TypeExprOptional t -> variant _TypeExpr_optional $ encodeType cx t
  TypeExprRecord fields -> variant _TypeExpr_record $ list $ fmap (encodeFieldType cx) fields
  TypeExprSet t -> variant _TypeExpr_set $ encodeType cx t
  TypeExprUnion fields -> variant _TypeExpr_union $ list $ fmap (encodeFieldType cx) fields
  TypeExprUniversal ut -> variant _TypeExpr_universal $ encodeUniversalType cx ut
  TypeExprVariable (TypeVariable var) -> variant _TypeExpr_variable $ string var

encodeTypeVariant :: Default m => Context m -> TypeVariant -> Term m
encodeTypeVariant cx tv = unitVariant $ case tv of
  TypeVariantLiteral -> _TypeVariant_literal
  TypeVariantElement -> _TypeVariant_element
  TypeVariantFunction -> _TypeVariant_function
  TypeVariantList -> _TypeVariant_list
  TypeVariantMap -> _TypeVariant_map
  TypeVariantNominal -> _TypeVariant_nominal
  TypeVariantOptional -> _TypeVariant_optional
  TypeVariantRecord -> _TypeVariant_record
  TypeVariantSet -> _TypeVariant_set
  TypeVariantUnion -> _TypeVariant_union
  TypeVariantUniversal -> _TypeVariant_universal
  TypeVariantVariable -> _TypeVariant_variable

encodeUniversalType :: Default m => Context m -> UniversalType m -> Term m
encodeUniversalType cx (UniversalType (TypeVariable var) body) = nominalRecord cx _UniversalType [
  Field _UniversalType_variable $ string var,
  Field _UniversalType_body $ encodeType cx body]
