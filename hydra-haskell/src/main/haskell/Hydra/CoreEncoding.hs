module Hydra.CoreEncoding where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Terms

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


encodeApplication :: (Ord m) => Context m -> Application m -> Term m
encodeApplication cx (Application lhs rhs) = nominalRecord cx _Application [
  Field _Application_function $ encodeTerm cx lhs,
  Field _Application_argument $ encodeTerm cx rhs]

encodeApplicationType :: Context m -> ApplicationType m -> Term m
encodeApplicationType cx (ApplicationType lhs rhs) = nominalRecord cx _ApplicationType [
  Field _ApplicationType_function $ encodeType cx lhs,
  Field _ApplicationType_argument $ encodeType cx rhs]

encodeElimination :: (Ord m) => Context m -> Elimination m -> Term m
encodeElimination cx e = case e of
  EliminationElement -> unitVariant _Elimination_element
  EliminationNominal (Name name) -> variant _Elimination_nominal $ string name
  EliminationOptional cases -> variant _Elimination_optional $ encodeOptionalCases cx cases
  EliminationRecord (FieldName fname) -> variant _Elimination_record $ string fname
  EliminationUnion cases -> variant _Elimination_union $ list $ encodeField cx <$> cases

encodeField :: (Ord m) => Context m -> Field m -> Term m
encodeField cx (Field (FieldName name) term) = nominalRecord cx _Field [
  Field _Field_name $ string name,
  Field _Field_term $ encodeTerm cx term]

encodeFieldType :: Context m -> FieldType m -> Term m
encodeFieldType cx (FieldType (FieldName fname) t) = nominalRecord cx _FieldType [
  Field _FieldType_name $ string fname,
  Field _FieldType_type $ encodeType cx t]

encodeFloatType :: FloatType -> Term m
encodeFloatType ft = unitVariant $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

encodeFunction :: (Ord m) => Context m -> Function m -> Term m
encodeFunction cx f = case f of
  FunctionCompareTo other -> variant _Function_compareTo $ encodeTerm cx other
  FunctionElimination e -> variant _Function_compareTo $ encodeElimination cx e
  FunctionLambda l -> variant _Function_lambda $ encodeLambda cx l
  FunctionPrimitive (Name name) -> variant _Function_primitive $ string name

encodeFunctionType :: Context m -> FunctionType m -> Term m
encodeFunctionType cx (FunctionType dom cod) = nominalRecord cx _FunctionType [
  Field _FunctionType_domain $ encodeType cx dom,
  Field _FunctionType_codomain $ encodeType cx cod]

encodeIntegerType :: IntegerType -> Term m
encodeIntegerType it = unitVariant $ case it of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

encodeLambda :: (Ord m) => Context m -> Lambda m -> Term m
encodeLambda cx (Lambda (Variable v) b) = nominalRecord cx _Lambda [
  Field _Lambda_parameter $ string v,
  Field _Lambda_body $ encodeTerm cx b]

encodeLambdaType :: Context m -> LambdaType m -> Term m
encodeLambdaType cx (LambdaType (VariableType var) body) = nominalRecord cx _LambdaType [
  Field _LambdaType_parameter $ string var,
  Field _LambdaType_body $ encodeType cx body]

encodeLiteralType :: LiteralType -> Term m
encodeLiteralType at = case at of
  LiteralTypeBinary -> unitVariant _LiteralType_binary
  LiteralTypeBoolean -> unitVariant _LiteralType_boolean
  LiteralTypeFloat ft -> variant _LiteralType_float $ encodeFloatType ft
  LiteralTypeInteger it -> variant _LiteralType_integer $ encodeIntegerType it
  LiteralTypeString -> unitVariant _LiteralType_string

encodeLiteral :: Context m -> Literal -> Term m
encodeLiteral _ = literal

encodeLiteralVariant :: LiteralVariant -> Term m
encodeLiteralVariant av = unitVariant $ case av of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

encodeMapType :: Context m -> MapType m -> Term m
encodeMapType cx (MapType kt vt) = nominalRecord cx _MapType [
  Field _MapType_keys $ encodeType cx kt,
  Field _MapType_values $ encodeType cx vt]

encodeNamed :: (Ord m) => Context m -> Named m -> Term m
encodeNamed cx (Named (Name name) term) = nominalRecord cx _Named [
  Field _Named_typeName $ string name,
  Field _Named_term $ encodeTerm cx term]

encodeOptionalCases :: (Ord m) => Context m -> OptionalCases m -> Term m
encodeOptionalCases cx (OptionalCases nothing just) = nominalRecord cx _OptionalCases [
  Field _OptionalCases_nothing $ encodeTerm cx nothing,
  Field _OptionalCases_just $ encodeTerm cx just]

encodeTerm :: (Ord m) => Context m -> Term m -> Term m
encodeTerm cx term = case term of
--  TermAnnotated a -> variant _Term_annotated $ encodeAnnotated cx a
  TermAnnotated (Annotated t ann) -> variant _Term_annotated $ TermAnnotated $ Annotated (encodeTerm cx t) ann
  TermApplication a -> variant _Term_application $ encodeApplication cx a
  TermLiteral av -> variant _Term_literal $ encodeLiteral cx av
  TermElement (Name name) -> variant _Term_element $ string name
  TermFunction f -> variant _Term_function $ encodeFunction cx f
  TermList terms -> variant _Term_list $ list $ encodeTerm cx <$> terms
  TermMap m -> variant _Term_map $ map $ M.fromList $ encodePair <$> M.toList m
    where encodePair (k, v) = (encodeTerm cx k, encodeTerm cx v)
  TermNominal ntt -> variant _Term_nominal $ encodeNamed cx ntt
  TermOptional m -> variant _Term_optional $ optional $ encodeTerm cx <$> m
  TermRecord fields -> variant _Term_record $ list $ encodeField cx <$> fields
  TermSet terms -> variant _Term_set $ set $ S.fromList $ encodeTerm cx <$> S.toList terms
  TermUnion field -> variant _Term_union $ encodeField cx field
  TermVariable (Variable var) -> variant _Term_variable $ string var

encodeType :: Context m -> Type m -> Term m
encodeType cx typ = case typ of
  TypeAnnotated (Annotated t ann) -> TermAnnotated (Annotated (encodeType cx t) ann)
  TypeApplication a -> variant _Type_application $ encodeApplicationType cx a
  TypeElement t -> variant _Type_element $ encodeType cx t
  TypeFunction ft -> variant _Type_function $ encodeFunctionType cx ft
  TypeLambda ut -> variant _Type_lambda $ encodeLambdaType cx ut
  TypeList t -> variant _Type_list $ encodeType cx t
  TypeLiteral at -> variant _Type_literal $ encodeLiteralType at
  TypeMap mt -> variant _Type_map $ encodeMapType cx mt
  TypeNominal name -> variant _Type_nominal $ element name
  TypeOptional t -> variant _Type_optional $ encodeType cx t
  TypeRecord fields -> variant _Type_record $ list $ fmap (encodeFieldType cx) fields
  TypeSet t -> variant _Type_set $ encodeType cx t
  TypeUnion fields -> variant _Type_union $ list $ fmap (encodeFieldType cx) fields
  TypeVariable (VariableType var) -> variant _Type_variable $ string var

encodeTypeVariant :: TypeVariant -> Term m
encodeTypeVariant tv = unitVariant $ case tv of
  TypeVariantAnnotated -> _TypeVariant_annotated
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
  TypeVariantLambda -> _TypeVariant_lambda
  TypeVariantVariable -> _TypeVariant_variable
