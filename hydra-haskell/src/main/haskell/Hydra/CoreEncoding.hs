module Hydra.CoreEncoding where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Terms

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


encodeApplication :: Ord m => Context m -> Application m -> Term m
encodeApplication cx (Application lhs rhs) = record _Application [
  Field _Application_function $ encodeTerm cx lhs,
  Field _Application_argument $ encodeTerm cx rhs]

encodeApplicationType :: Context m -> ApplicationType m -> Term m
encodeApplicationType cx (ApplicationType lhs rhs) = record _ApplicationType [
  Field _ApplicationType_function $ encodeType cx lhs,
  Field _ApplicationType_argument $ encodeType cx rhs]

encodeCaseStatement :: Ord m => Context m -> CaseStatement m -> Term m
encodeCaseStatement cx (CaseStatement name cases) = record _CaseStatement [
  Field _CaseStatement_typeName $ string (unName name),
  Field _CaseStatement_cases $ list $ encodeField cx <$> cases]

encodeElimination :: Ord m => Context m -> Elimination m -> Term m
encodeElimination cx e = case e of
  EliminationElement -> unitVariant _Elimination _Elimination_element
  EliminationNominal (Name name) -> variant _Elimination _Elimination_nominal $ string name
  EliminationOptional cases -> variant _Elimination _Elimination_optional $ encodeOptionalCases cx cases
  EliminationRecord p -> variant _Elimination _Elimination_record $ encodeProjection cx p
  EliminationUnion c -> variant _Elimination _Elimination_union $ encodeCaseStatement cx c

encodeField :: Ord m => Context m -> Field m -> Term m
encodeField cx (Field (FieldName name) term) = record _Field [
  Field _Field_name $ string name,
  Field _Field_term $ encodeTerm cx term]

encodeFieldType :: Context m -> FieldType m -> Term m
encodeFieldType cx (FieldType (FieldName fname) t) = record _FieldType [
  Field _FieldType_name $ string fname,
  Field _FieldType_type $ encodeType cx t]

encodeFloatType :: FloatType -> Term m
encodeFloatType ft = unitVariant _FloatType $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

encodeFunction :: Ord m => Context m -> Function m -> Term m
encodeFunction cx f = case f of
  FunctionCompareTo other -> variant _Function _Function_compareTo $ encodeTerm cx other
  FunctionElimination e -> variant _Function _Function_compareTo $ encodeElimination cx e
  FunctionLambda l -> variant _Function _Function_lambda $ encodeLambda cx l
  FunctionPrimitive (Name name) -> variant _Function _Function_primitive $ string name

encodeFunctionType :: Context m -> FunctionType m -> Term m
encodeFunctionType cx (FunctionType dom cod) = record _FunctionType [
  Field _FunctionType_domain $ encodeType cx dom,
  Field _FunctionType_codomain $ encodeType cx cod]

encodeIntegerType :: IntegerType -> Term m
encodeIntegerType it = unitVariant _IntegerType $ case it of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

encodeLambda :: Ord m => Context m -> Lambda m -> Term m
encodeLambda cx (Lambda (Variable v) b) = record _Lambda [
  Field _Lambda_parameter $ string v,
  Field _Lambda_body $ encodeTerm cx b]

encodeLambdaType :: Context m -> LambdaType m -> Term m
encodeLambdaType cx (LambdaType (VariableType var) body) = record _LambdaType [
  Field _LambdaType_parameter $ string var,
  Field _LambdaType_body $ encodeType cx body]

encodeLiteralType :: LiteralType -> Term m
encodeLiteralType at = case at of
  LiteralTypeBinary -> unitVariant _LiteralType _LiteralType_binary
  LiteralTypeBoolean -> unitVariant _LiteralType _LiteralType_boolean
  LiteralTypeFloat ft -> variant _LiteralType _LiteralType_float $ encodeFloatType ft
  LiteralTypeInteger it -> variant _LiteralType _LiteralType_integer $ encodeIntegerType it
  LiteralTypeString -> unitVariant _LiteralType _LiteralType_string

encodeLiteral :: Context m -> Literal -> Term m
encodeLiteral _ = literal

encodeLiteralVariant :: LiteralVariant -> Term m
encodeLiteralVariant av = unitVariant _LiteralVariant $ case av of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

encodeMapType :: Context m -> MapType m -> Term m
encodeMapType cx (MapType kt vt) = record _MapType [
  Field _MapType_keys $ encodeType cx kt,
  Field _MapType_values $ encodeType cx vt]

encodeNamed :: Ord m => Context m -> Named m -> Term m
encodeNamed cx (Named (Name name) term) = record _Named [
  Field _Named_typeName $ string name,
  Field _Named_term $ encodeTerm cx term]

encodeOptionalCases :: Ord m => Context m -> OptionalCases m -> Term m
encodeOptionalCases cx (OptionalCases nothing just) = record _OptionalCases [
  Field _OptionalCases_nothing $ encodeTerm cx nothing,
  Field _OptionalCases_just $ encodeTerm cx just]

encodeProjection :: Context m -> Projection -> Term m
encodeProjection cx (Projection name fname) = record _Projection [
  Field _Projection_typeName $ string (unName name),
  Field _Projection_field $ string (unFieldName fname)]
  
encodeRowType :: Context m -> RowType m -> Term m
encodeRowType cx (RowType name fields) = record _RowType [
  Field _RowType_typeName $ string (unName name),
  Field _RowType_fields $ list $ encodeFieldType cx <$> fields]

encodeTerm :: Ord m => Context m -> Term m -> Term m
encodeTerm cx term = case term of
--  TermAnnotated a -> variant _Term_annotated $ encodeAnnotated cx a
  TermAnnotated (Annotated t ann) -> variant _Term _Term_annotated $ TermAnnotated $ Annotated (encodeTerm cx t) ann
  TermApplication a -> variant _Term _Term_application $ encodeApplication cx a
  TermLiteral av -> variant _Term _Term_literal $ encodeLiteral cx av
  TermElement (Name name) -> variant _Term _Term_element $ string name
  TermFunction f -> variant _Term _Term_function $ encodeFunction cx f
  TermList terms -> variant _Term _Term_list $ list $ encodeTerm cx <$> terms
  TermMap m -> variant _Term _Term_map $ map $ M.fromList $ encodePair <$> M.toList m
    where encodePair (k, v) = (encodeTerm cx k, encodeTerm cx v)
  TermNominal ntt -> variant _Term _Term_nominal $ encodeNamed cx ntt
  TermOptional m -> variant _Term _Term_optional $ optional $ encodeTerm cx <$> m
  TermRecord (Record _ fields) -> variant _Term _Term_record $ list $ encodeField cx <$> fields
  TermSet terms -> variant _Term _Term_set $ set $ S.fromList $ encodeTerm cx <$> S.toList terms
  TermUnion (Union _ field) -> variant _Term _Term_union $ encodeField cx field
  TermVariable (Variable var) -> variant _Term _Term_variable $ string var

encodeType :: Context m -> Type m -> Term m
encodeType cx typ = case typ of
  TypeAnnotated (Annotated t ann) -> TermAnnotated (Annotated (encodeType cx t) ann)
  TypeApplication a -> variant _Type _Type_application $ encodeApplicationType cx a
  TypeElement t -> variant _Type _Type_element $ encodeType cx t
  TypeFunction ft -> variant _Type _Type_function $ encodeFunctionType cx ft
  TypeLambda ut -> variant _Type _Type_lambda $ encodeLambdaType cx ut
  TypeList t -> variant _Type _Type_list $ encodeType cx t
  TypeLiteral at -> variant _Type _Type_literal $ encodeLiteralType at
  TypeMap mt -> variant _Type _Type_map $ encodeMapType cx mt
  TypeNominal name -> variant _Type _Type_nominal $ element name
  TypeOptional t -> variant _Type _Type_optional $ encodeType cx t
  TypeRecord rt -> variant _Type _Type_record $ encodeRowType cx rt
  TypeSet t -> variant _Type _Type_set $ encodeType cx t
  TypeUnion rt -> variant _Type _Type_union $ encodeRowType cx rt
  TypeVariable (VariableType var) -> variant _Type _Type_variable $ string var

encodeTypeVariant :: TypeVariant -> Term m
encodeTypeVariant tv = unitVariant _TypeVariant $ case tv of
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
