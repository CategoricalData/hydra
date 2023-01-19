-- | Encoding of types as terms

module Hydra.CoreEncoding where

import Hydra.Core
import Hydra.Compute
import Hydra.Mantle
import Hydra.Monads
import Hydra.Impl.Haskell.Dsl.Terms

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


encodeApplication :: Ord m => Application m -> Term m
encodeApplication (Application lhs rhs) = record _Application [
  Field _Application_function $ encodeTerm lhs,
  Field _Application_argument $ encodeTerm rhs]

encodeApplicationType :: ApplicationType m -> Term m
encodeApplicationType (ApplicationType lhs rhs) = record _ApplicationType [
  Field _ApplicationType_function $ encodeType lhs,
  Field _ApplicationType_argument $ encodeType rhs]

encodeCaseStatement :: Ord m => CaseStatement m -> Term m
encodeCaseStatement (CaseStatement name cases) = record _CaseStatement [
  Field _CaseStatement_typeName $ string (unName name),
  Field _CaseStatement_cases $ list $ encodeField <$> cases]

encodeElimination :: Ord m => Elimination m -> Term m
encodeElimination e = case e of
  EliminationElement -> unitVariant _Elimination _Elimination_element
  EliminationList f -> variant _Elimination _Elimination_list $ encodeTerm f
  EliminationWrapped (Name name) -> variant _Elimination _Elimination_wrapped $ string name
  EliminationOptional cases -> variant _Elimination _Elimination_optional $ encodeOptionalCases cases
  EliminationRecord p -> variant _Elimination _Elimination_record $ encodeProjection p
  EliminationUnion c -> variant _Elimination _Elimination_union $ encodeCaseStatement c

encodeField :: Ord m => Field m -> Term m
encodeField (Field (FieldName name) term) = record _Field [
  Field _Field_name $ string name,
  Field _Field_term $ encodeTerm term]

encodeFieldType :: FieldType m -> Term m
encodeFieldType (FieldType (FieldName fname) t) = record _FieldType [
  Field _FieldType_name $ string fname,
  Field _FieldType_type $ encodeType t]

encodeFloatType :: FloatType -> Term m
encodeFloatType ft = unitVariant _FloatType $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

encodeFunction :: Ord m => Function m -> Term m
encodeFunction f = case f of
  FunctionCompareTo other -> variant _Function _Function_compareTo $ encodeTerm other
  FunctionElimination e -> variant _Function _Function_compareTo $ encodeElimination e
  FunctionLambda l -> variant _Function _Function_lambda $ encodeLambda l
  FunctionPrimitive (Name name) -> variant _Function _Function_primitive $ string name

encodeFunctionType :: FunctionType m -> Term m
encodeFunctionType (FunctionType dom cod) = record _FunctionType [
  Field _FunctionType_domain $ encodeType dom,
  Field _FunctionType_codomain $ encodeType cod]

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

encodeLambda :: Ord m => Lambda m -> Term m
encodeLambda (Lambda (Variable v) b) = record _Lambda [
  Field _Lambda_parameter $ string v,
  Field _Lambda_body $ encodeTerm b]

encodeLambdaType :: LambdaType m -> Term m
encodeLambdaType (LambdaType (VariableType var) body) = record _LambdaType [
  Field _LambdaType_parameter $ string var,
  Field _LambdaType_body $ encodeType body]

encodeLiteralType :: LiteralType -> Term m
encodeLiteralType at = case at of
  LiteralTypeBinary -> unitVariant _LiteralType _LiteralType_binary
  LiteralTypeBoolean -> unitVariant _LiteralType _LiteralType_boolean
  LiteralTypeFloat ft -> variant _LiteralType _LiteralType_float $ encodeFloatType ft
  LiteralTypeInteger it -> variant _LiteralType _LiteralType_integer $ encodeIntegerType it
  LiteralTypeString -> unitVariant _LiteralType _LiteralType_string

encodeLiteral :: Literal -> Term m
encodeLiteral = literal

encodeLiteralVariant :: LiteralVariant -> Term m
encodeLiteralVariant av = unitVariant _LiteralVariant $ case av of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

encodeMapType :: MapType m -> Term m
encodeMapType (MapType kt vt) = record _MapType [
  Field _MapType_keys $ encodeType kt,
  Field _MapType_values $ encodeType vt]

encodeWrapper :: Ord m => Wrapper m -> Term m
encodeWrapper (Wrapper (Name name) term) = record _Wrapper [
  Field _Wrapper_typeName $ string name,
  Field _Wrapper_term $ encodeTerm term]

encodeOptionalCases :: Ord m => OptionalCases m -> Term m
encodeOptionalCases (OptionalCases nothing just) = record _OptionalCases [
  Field _OptionalCases_nothing $ encodeTerm nothing,
  Field _OptionalCases_just $ encodeTerm just]

encodeProjection :: Projection -> Term m
encodeProjection (Projection name fname) = record _Projection [
  Field _Projection_typeName $ string (unName name),
  Field _Projection_field $ string (unFieldName fname)]

encodeRowType :: RowType m -> Term m
encodeRowType (RowType name extends fields) = record _RowType [
  Field _RowType_typeName $ string (unName name),
  Field _RowType_extends $ optional (string . unName <$> extends),
  Field _RowType_fields $ list $ encodeFieldType <$> fields]

encodeSum :: Ord m => Sum m -> Term m
encodeSum (Sum i l term) = record _Sum [
  Field _Sum_index $ int32 i,
  Field _Sum_size $ int32 l,
  Field _Sum_term $ encodeTerm term]

encodeTerm :: Ord m => Term m -> Term m
encodeTerm term = case term of
  TermAnnotated (Annotated t ann) -> variant _Term _Term_annotated $ TermAnnotated $ Annotated (encodeTerm t) ann
  TermApplication a -> variant _Term _Term_application $ encodeApplication a
  TermLiteral av -> variant _Term _Term_literal $ encodeLiteral av
  TermElement (Name name) -> variant _Term _Term_element $ string name
  TermFunction f -> variant _Term _Term_function $ encodeFunction f
  TermList terms -> variant _Term _Term_list $ list $ encodeTerm <$> terms
  TermMap m -> variant _Term _Term_map $ map $ M.fromList $ encodePair <$> M.toList m
    where encodePair (k, v) = (encodeTerm k, encodeTerm v)
  TermWrapped ntt -> variant _Term _Term_wrapped $ encodeWrapper ntt
  TermOptional m -> variant _Term _Term_optional $ optional $ encodeTerm <$> m
  TermProduct terms -> variant _Term _Term_product $ list (encodeTerm <$> terms)
  TermRecord (Record _ fields) -> variant _Term _Term_record $ list $ encodeField <$> fields
  TermSet terms -> variant _Term _Term_set $ set $ S.fromList $ encodeTerm <$> S.toList terms
  TermSum s -> variant _Term _Term_sum $ encodeSum s
  TermUnion (Union _ field) -> variant _Term _Term_union $ encodeField field
  TermVariable (Variable var) -> variant _Term _Term_variable $ string var

encodeType :: Type m -> Term m
encodeType typ = case typ of
  TypeAnnotated (Annotated t ann) -> TermAnnotated (Annotated (encodeType t) ann)
  TypeApplication a -> variant _Type _Type_application $ encodeApplicationType a
  TypeElement t -> variant _Type _Type_element $ encodeType t
  TypeFunction ft -> variant _Type _Type_function $ encodeFunctionType ft
  TypeLambda ut -> variant _Type _Type_lambda $ encodeLambdaType ut
  TypeList t -> variant _Type _Type_list $ encodeType t
  TypeLiteral at -> variant _Type _Type_literal $ encodeLiteralType at
  TypeMap mt -> variant _Type _Type_map $ encodeMapType mt
  TypeWrapped name -> variant _Type _Type_wrapped $ element name
  TypeOptional t -> variant _Type _Type_optional $ encodeType t
  TypeProduct types -> variant _Type _Type_product $ list (encodeType <$> types)
  TypeRecord rt -> variant _Type _Type_record $ encodeRowType rt
  TypeSet t -> variant _Type _Type_set $ encodeType t
  TypeSum types -> variant _Type _Type_sum $ list (encodeType <$> types)
  TypeUnion rt -> variant _Type _Type_union $ encodeRowType rt
  TypeVariable (VariableType var) -> variant _Type _Type_variable $ string var

encodeTypeVariant :: TypeVariant -> Term m
encodeTypeVariant tv = unitVariant _TypeVariant $ case tv of
  TypeVariantAnnotated -> _TypeVariant_annotated
  TypeVariantLiteral -> _TypeVariant_literal
  TypeVariantElement -> _TypeVariant_element
  TypeVariantFunction -> _TypeVariant_function
  TypeVariantList -> _TypeVariant_list
  TypeVariantMap -> _TypeVariant_map
  TypeVariantWrapped -> _TypeVariant_wrapped
  TypeVariantOptional -> _TypeVariant_optional
  TypeVariantProduct -> _TypeVariant_product
  TypeVariantRecord -> _TypeVariant_record
  TypeVariantSet -> _TypeVariant_set
  TypeVariantSum -> _TypeVariant_sum
  TypeVariantUnion -> _TypeVariant_union
  TypeVariantLambda -> _TypeVariant_lambda
  TypeVariantVariable -> _TypeVariant_variable
