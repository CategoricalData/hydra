-- | Encoding of types as terms (epsilon encoding), and terms as simplified terms (sigma encoding)

module Hydra.CoreEncoding where

import Hydra.Core
import Hydra.Compute
import Hydra.Mantle
import Hydra.Monads
import Hydra.Dsl.Terms as Terms

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


epsilonEncodeApplicationType :: ApplicationType m -> Term m
epsilonEncodeApplicationType (ApplicationType lhs rhs) = record _ApplicationType [
  Field _ApplicationType_function $ epsilonEncodeType lhs,
  Field _ApplicationType_argument $ epsilonEncodeType rhs]

epsilonEncodeFieldType :: FieldType m -> Term m
epsilonEncodeFieldType (FieldType (FieldName fname) t) = record _FieldType [
  Field _FieldType_name $ string fname,
  Field _FieldType_type $ epsilonEncodeType t]

epsilonEncodeFloatType :: FloatType -> Term m
epsilonEncodeFloatType ft = unitVariant _FloatType $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

epsilonEncodeFunctionType :: FunctionType m -> Term m
epsilonEncodeFunctionType (FunctionType dom cod) = record _FunctionType [
  Field _FunctionType_domain $ epsilonEncodeType dom,
  Field _FunctionType_codomain $ epsilonEncodeType cod]

epsilonEncodeIntegerType :: IntegerType -> Term m
epsilonEncodeIntegerType it = unitVariant _IntegerType $ case it of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

epsilonEncodeLambdaType :: LambdaType m -> Term m
epsilonEncodeLambdaType (LambdaType (Name var) body) = record _LambdaType [
  Field _LambdaType_parameter $ string var,
  Field _LambdaType_body $ epsilonEncodeType body]

epsilonEncodeLiteralType :: LiteralType -> Term m
epsilonEncodeLiteralType at = case at of
  LiteralTypeBinary -> unitVariant _LiteralType _LiteralType_binary
  LiteralTypeBoolean -> unitVariant _LiteralType _LiteralType_boolean
  LiteralTypeFloat ft -> variant _LiteralType _LiteralType_float $ epsilonEncodeFloatType ft
  LiteralTypeInteger it -> variant _LiteralType _LiteralType_integer $ epsilonEncodeIntegerType it
  LiteralTypeString -> unitVariant _LiteralType _LiteralType_string

epsilonEncodeMapType :: MapType m -> Term m
epsilonEncodeMapType (MapType kt vt) = record _MapType [
  Field _MapType_keys $ epsilonEncodeType kt,
  Field _MapType_values $ epsilonEncodeType vt]

epsilonEncodeRowType :: RowType m -> Term m
epsilonEncodeRowType (RowType name extends fields) = record _RowType [
  Field _RowType_typeName $ string (unName name),
  Field _RowType_extends $ optional (string . unName <$> extends),
  Field _RowType_fields $ list $ epsilonEncodeFieldType <$> fields]

epsilonEncodeType :: Type m -> Term m
epsilonEncodeType typ = case typ of
  TypeAnnotated (Annotated t ann) -> TermAnnotated (Annotated (epsilonEncodeType t) ann)
  TypeApplication a -> variant _Type _Type_application $ epsilonEncodeApplicationType a
  TypeElement t -> variant _Type _Type_element $ epsilonEncodeType t
  TypeFunction ft -> variant _Type _Type_function $ epsilonEncodeFunctionType ft
  TypeLambda ut -> variant _Type _Type_lambda $ epsilonEncodeLambdaType ut
  TypeList t -> variant _Type _Type_list $ epsilonEncodeType t
  TypeLiteral at -> variant _Type _Type_literal $ epsilonEncodeLiteralType at
  TypeMap mt -> variant _Type _Type_map $ epsilonEncodeMapType mt
  TypeOptional t -> variant _Type _Type_optional $ epsilonEncodeType t
  TypeProduct types -> variant _Type _Type_product $ list (epsilonEncodeType <$> types)
  TypeRecord rt -> variant _Type _Type_record $ epsilonEncodeRowType rt
  TypeSet t -> variant _Type _Type_set $ epsilonEncodeType t
  TypeSum types -> variant _Type _Type_sum $ list (epsilonEncodeType <$> types)
  TypeUnion rt -> variant _Type _Type_union $ epsilonEncodeRowType rt
  TypeVariable (Name var) -> variant _Type _Type_variable $ string var
  TypeWrap name -> variant _Type _Type_wrap $ element name

sigmaEncodeApplication :: Ord m => Application m -> Term m
sigmaEncodeApplication (Application lhs rhs) = record _Application [
  Field _Application_function $ sigmaEncodeTerm lhs,
  Field _Application_argument $ sigmaEncodeTerm rhs]

sigmaEncodeCaseStatement :: Ord m => CaseStatement m -> Term m
sigmaEncodeCaseStatement (CaseStatement name cases) = record _CaseStatement [
  Field _CaseStatement_typeName $ string (unName name),
  Field _CaseStatement_cases $ list $ sigmaEncodeField <$> cases]

sigmaEncodeElimination :: Ord m => Elimination m -> Term m
sigmaEncodeElimination e = case e of
  EliminationElement -> unitVariant _Elimination _Elimination_element
  EliminationList f -> variant _Elimination _Elimination_list $ sigmaEncodeTerm f
  EliminationWrap (Name name) -> variant _Elimination _Elimination_wrap $ string name
  EliminationOptional cases -> variant _Elimination _Elimination_optional $ sigmaEncodeOptionalCases cases
  EliminationRecord p -> variant _Elimination _Elimination_record $ sigmaEncodeProjection p
  EliminationUnion c -> variant _Elimination _Elimination_union $ sigmaEncodeCaseStatement c

sigmaEncodeField :: Ord m => Field m -> Term m
sigmaEncodeField (Field (FieldName name) term) = record _Field [
  Field _Field_name $ string name,
  Field _Field_term $ sigmaEncodeTerm term]

sigmaEncodeFunction :: Ord m => Function m -> Term m
sigmaEncodeFunction f = case f of
  FunctionElimination e -> variant _Function _Function_elimination $ sigmaEncodeElimination e
  FunctionLambda l -> variant _Function _Function_lambda $ sigmaEncodeLambda l
  FunctionPrimitive (Name name) -> variant _Function _Function_primitive $ string name

sigmaEncodeLambda :: Ord m => Lambda m -> Term m
sigmaEncodeLambda (Lambda (Name v) b) = record _Lambda [
  Field _Lambda_parameter $ string v,
  Field _Lambda_body $ sigmaEncodeTerm b]

sigmaEncodeLiteral :: Literal -> Term m
sigmaEncodeLiteral = literal

sigmaEncodeLiteralVariant :: LiteralVariant -> Term m
sigmaEncodeLiteralVariant av = unitVariant _LiteralVariant $ case av of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

sigmaEncodeNominalTerm :: Ord m => Nominal (Term m) -> Term m
sigmaEncodeNominalTerm (Nominal (Name name) term) = record _Nominal [
  Field _Nominal_typeName $ string name,
  Field _Nominal_object $ sigmaEncodeTerm term]

sigmaEncodeOptionalCases :: Ord m => OptionalCases m -> Term m
sigmaEncodeOptionalCases (OptionalCases nothing just) = record _OptionalCases [
  Field _OptionalCases_nothing $ sigmaEncodeTerm nothing,
  Field _OptionalCases_just $ sigmaEncodeTerm just]

sigmaEncodeProjection :: Projection -> Term m
sigmaEncodeProjection (Projection name fname) = record _Projection [
  Field _Projection_typeName $ string (unName name),
  Field _Projection_field $ string (unFieldName fname)]

sigmaEncodeSum :: Ord m => Sum m -> Term m
sigmaEncodeSum (Sum i l term) = record _Sum [
  Field _Sum_index $ int32 i,
  Field _Sum_size $ int32 l,
  Field _Sum_term $ sigmaEncodeTerm term]

sigmaEncodeTerm :: Ord m => Term m -> Term m
sigmaEncodeTerm term = case term of
  TermAnnotated (Annotated t ann) -> variant _Term _Term_annotated $ TermAnnotated $ Annotated (sigmaEncodeTerm t) ann
  TermApplication a -> variant _Term _Term_application $ sigmaEncodeApplication a
  TermLiteral av -> variant _Term _Term_literal $ sigmaEncodeLiteral av
  TermElement (Name name) -> variant _Term _Term_element $ string name
  TermFunction f -> variant _Term _Term_function $ sigmaEncodeFunction f
  TermList terms -> variant _Term _Term_list $ list $ sigmaEncodeTerm <$> terms
  TermMap m -> variant _Term _Term_map $ map $ M.fromList $ sigmaEncodePair <$> M.toList m
    where sigmaEncodePair (k, v) = (sigmaEncodeTerm k, sigmaEncodeTerm v)
  TermOptional m -> variant _Term _Term_optional $ optional $ sigmaEncodeTerm <$> m
  TermProduct terms -> variant _Term _Term_product $ list (sigmaEncodeTerm <$> terms)
  TermRecord (Record _ fields) -> variant _Term _Term_record $ list $ sigmaEncodeField <$> fields
  TermSet terms -> variant _Term _Term_set $ set $ S.fromList $ sigmaEncodeTerm <$> S.toList terms
  TermSum s -> variant _Term _Term_sum $ sigmaEncodeSum s
  TermUnion (Injection _ field) -> variant _Term _Term_union $ sigmaEncodeField field
  TermVariable (Name var) -> variant _Term _Term_variable $ string var
  TermWrap ntt -> variant _Term _Term_wrap $ sigmaEncodeNominalTerm ntt
