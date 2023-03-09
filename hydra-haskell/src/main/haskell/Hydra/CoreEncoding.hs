-- | Encoding of types as terms (epsilon encoding), and terms as simplified terms (sigma encoding)

module Hydra.CoreEncoding where

import Hydra.Basics
import Hydra.Core
import Hydra.Mantle
import Hydra.Dsl.Terms as Terms

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.Set as S


epsilonEncodeApplicationType :: ApplicationType a -> Term a
epsilonEncodeApplicationType (ApplicationType lhs rhs) = record _ApplicationType [
  Field _ApplicationType_function $ epsilonEncodeType lhs,
  Field _ApplicationType_argument $ epsilonEncodeType rhs]

epsilonEncodeFieldType :: FieldType a -> Term a
epsilonEncodeFieldType (FieldType (FieldName fname) t) = record _FieldType [
  Field _FieldType_name $ string fname,
  Field _FieldType_type $ epsilonEncodeType t]

epsilonEncodeFloatType :: FloatType -> Term a
epsilonEncodeFloatType ft = unitVariant _FloatType $ case ft of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

epsilonEncodeFunctionType :: FunctionType a -> Term a
epsilonEncodeFunctionType (FunctionType dom cod) = record _FunctionType [
  Field _FunctionType_domain $ epsilonEncodeType dom,
  Field _FunctionType_codomain $ epsilonEncodeType cod]

epsilonEncodeIntegerType :: IntegerType -> Term a
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

epsilonEncodeLambdaType :: LambdaType a -> Term a
epsilonEncodeLambdaType (LambdaType (Name var) body) = record _LambdaType [
  Field _LambdaType_parameter $ string var,
  Field _LambdaType_body $ epsilonEncodeType body]

epsilonEncodeLiteralType :: LiteralType -> Term a
epsilonEncodeLiteralType at = case at of
  LiteralTypeBinary -> unitVariant _LiteralType _LiteralType_binary
  LiteralTypeBoolean -> unitVariant _LiteralType _LiteralType_boolean
  LiteralTypeFloat ft -> variant _LiteralType _LiteralType_float $ epsilonEncodeFloatType ft
  LiteralTypeInteger it -> variant _LiteralType _LiteralType_integer $ epsilonEncodeIntegerType it
  LiteralTypeString -> unitVariant _LiteralType _LiteralType_string

epsilonEncodeMapType :: MapType a -> Term a
epsilonEncodeMapType (MapType kt vt) = record _MapType [
  Field _MapType_keys $ epsilonEncodeType kt,
  Field _MapType_values $ epsilonEncodeType vt]

epsilonEncodeRowType :: RowType a -> Term a
epsilonEncodeRowType (RowType name extends fields) = record _RowType [
  Field _RowType_typeName $ string (unName name),
  Field _RowType_extends $ optional (string . unName <$> extends),
  Field _RowType_fields $ list $ epsilonEncodeFieldType <$> fields]

epsilonEncodeType :: Type a -> Term a
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
  TypeWrap name -> variant _Type _Type_wrap $ TermElement name

sigmaEncodeApplication :: Ord a => Application a -> Term a
sigmaEncodeApplication (Application lhs rhs) = record _Application [
  Field _Application_function $ sigmaEncodeTerm lhs,
  Field _Application_argument $ sigmaEncodeTerm rhs]

sigmaEncodeCaseStatement :: Ord a => CaseStatement a -> Term a
sigmaEncodeCaseStatement (CaseStatement name cases def) = record _CaseStatement [
  Field _CaseStatement_typeName $ string (unName name),
  Field _CaseStatement_cases $ list $ sigmaEncodeField <$> cases,
  Field _CaseStatement_default $ optional $ sigmaEncodeTerm <$> def]

sigmaEncodeElimination :: Ord a => Elimination a -> Term a
sigmaEncodeElimination e = case e of
  EliminationElement -> unitVariant _Elimination _Elimination_element
  EliminationList f -> variant _Elimination _Elimination_list $ sigmaEncodeTerm f
  EliminationWrap (Name name) -> variant _Elimination _Elimination_wrap $ string name
  EliminationOptional cases -> variant _Elimination _Elimination_optional $ sigmaEncodeOptionalCases cases
  EliminationRecord p -> variant _Elimination _Elimination_record $ sigmaEncodeProjection p
  EliminationUnion c -> variant _Elimination _Elimination_union $ sigmaEncodeCaseStatement c

sigmaEncodeField :: Ord a => Field a -> Term a
sigmaEncodeField (Field (FieldName name) term) = record _Field [
  Field _Field_name $ string name,
  Field _Field_term $ sigmaEncodeTerm term]

sigmaEncodeFloatValue :: FloatValue -> Term a
sigmaEncodeFloatValue f = variant _FloatValue var $ float f
  where
    var = case floatValueType f of
      FloatTypeBigfloat -> _FloatType_bigfloat
      FloatTypeFloat32 -> _FloatType_float32
      FloatTypeFloat64 -> _FloatType_float64

sigmaEncodeFunction :: Ord a => Function a -> Term a
sigmaEncodeFunction f = case f of
  FunctionElimination e -> variant _Function _Function_elimination $ sigmaEncodeElimination e
  FunctionLambda l -> variant _Function _Function_lambda $ sigmaEncodeLambda l
  FunctionPrimitive (Name name) -> variant _Function _Function_primitive $ TermWrap $ Nominal _Name $ string name

sigmaEncodeIntegerValue :: IntegerValue -> Term a
sigmaEncodeIntegerValue i = variant _IntegerValue var $ integer i
  where
    var = case integerValueType i of
      IntegerTypeBigint -> _IntegerType_bigint
      IntegerTypeInt8 -> _IntegerType_int8
      IntegerTypeInt16 -> _IntegerType_int16
      IntegerTypeInt32 -> _IntegerType_int32
      IntegerTypeInt64 -> _IntegerType_int64
      IntegerTypeUint8 -> _IntegerType_uint8
      IntegerTypeUint16 -> _IntegerType_uint16
      IntegerTypeUint32 -> _IntegerType_uint32
      IntegerTypeUint64 -> _IntegerType_uint64

sigmaEncodeLambda :: Ord a => Lambda a -> Term a
sigmaEncodeLambda (Lambda (Name v) b) = record _Lambda [
  Field _Lambda_parameter $ string v,
  Field _Lambda_body $ sigmaEncodeTerm b]

sigmaEncodeLiteral :: Literal -> Term a
sigmaEncodeLiteral l = case l of
  LiteralBinary v -> variant _Literal _LiteralVariant_binary $ string v
  LiteralBoolean v -> variant _Literal _LiteralVariant_boolean $ boolean v
  LiteralFloat v -> variant _Literal _LiteralVariant_float $ sigmaEncodeFloatValue v
  LiteralInteger v -> variant _Literal _LiteralVariant_integer $ sigmaEncodeIntegerValue v
  LiteralString v -> variant _Literal _LiteralVariant_string $ string v

sigmaEncodeLiteralVariant :: LiteralVariant -> Term a
sigmaEncodeLiteralVariant av = unitVariant _LiteralVariant $ case av of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

sigmaEncodeNominalTerm :: Ord a => Nominal (Term a) -> Term a
sigmaEncodeNominalTerm (Nominal (Name name) term) = record _Nominal [
  Field _Nominal_typeName $ string name,
  Field _Nominal_object $ sigmaEncodeTerm term]

sigmaEncodeOptionalCases :: Ord a => OptionalCases a -> Term a
sigmaEncodeOptionalCases (OptionalCases nothing just) = record _OptionalCases [
  Field _OptionalCases_nothing $ sigmaEncodeTerm nothing,
  Field _OptionalCases_just $ sigmaEncodeTerm just]

sigmaEncodeProjection :: Projection -> Term a
sigmaEncodeProjection (Projection name fname) = record _Projection [
  Field _Projection_typeName $ string (unName name),
  Field _Projection_field $ string (unFieldName fname)]

sigmaEncodeSum :: Ord a => Sum a -> Term a
sigmaEncodeSum (Sum i l term) = record _Sum [
  Field _Sum_index $ int32 i,
  Field _Sum_size $ int32 l,
  Field _Sum_term $ sigmaEncodeTerm term]

sigmaEncodeTerm :: Ord a => Term a -> Term a
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
