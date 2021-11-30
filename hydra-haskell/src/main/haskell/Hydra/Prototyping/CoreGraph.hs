module Hydra.Prototyping.CoreGraph (
  hydraCoreGraph,
   
  hcApplication,
  hcBooleanValue,
  hcComparison,
  hcField,
  hcFieldName,
  hcFieldType,
  hcFloatType,
  hcFloatValue,
  hcFloatVariant,
  hcFunction,
  hcFunctionType,
  hcFunctionVariant,
  hcIntegerType,
  hcIntegerValue,
  hcIntegerVariant,
  hcLambda,
  hcLiteral,
  hcLiteralType,
  hcLiteralVariant,
  hcMapType,
  hcName,
  hcPrecision,
  hcTerm,
  hcTermVariant,
  hcType,
  hcTypeVariable,
  hcTypeVariant,
  hcTypedTerm,
  hcUniversalType,
  hcVariable,
   ) where

import Hydra.Core
import Hydra.Graph
import Hydra.Prototyping.CoreEncoding
import Hydra.Impl.Haskell.Dsl


-- Note: here, the element namespace "hydra/core" doubles as a graph name
hydraCoreGraph :: Graph Meta
hydraCoreGraph = Graph "hydra/core" elements (const True) "hydra/core"
  where
    elements = [
      hcApplication,
      hcBooleanValue,
      hcComparison,
      hcField,
      hcFieldName,
      hcFieldType,
      hcFloatType,
      hcFloatValue,
      hcFloatVariant,
      hcFunction,
      hcFunctionType,
      hcFunctionVariant,
      hcIntegerType,
      hcIntegerValue,
      hcIntegerVariant,
      hcLambda,
      hcLiteral,
      hcLiteralType,
      hcLiteralVariant,
      hcMapType,
      hcName,
      hcPrecision,
      hcTerm,
      hcTermVariant,
      hcType,
      hcTypeVariable,
      hcTypeVariant,
      hcTypedTerm,
      hcUniversalType,
      hcVariable]

typeElement :: Name -> Type -> Element Meta
typeElement name typ = Element {
  elementName = name,
  elementSchema = defaultTerm $ ExpressionElement _Type,
  elementData = encodeType typ}

enum :: [FieldName] -> Type
enum names = TypeUnion $ (`FieldType` unitType) <$> names

hcApplication :: Element Meta
hcApplication = typeElement _Application $ TypeRecord [
  FieldType _Application_function $ TypeNominal _Term,
  FieldType _Application_argument $ TypeNominal _Term]

hcBooleanValue :: Element Meta
hcBooleanValue = typeElement _BooleanValue $ enum [
  _BooleanValue_false,
  _BooleanValue_true]

hcComparison :: Element Meta
hcComparison = typeElement _Comparison $ enum [
  _Comparison_lessThan,
  _Comparison_equalTo,
  _Comparison_greaterThan]

hcField :: Element Meta
hcField = typeElement _Field $ TypeRecord [
  FieldType _Field_name $ TypeNominal _FieldName,
  FieldType _Field_term $ TypeNominal _Term]

hcFieldName :: Element Meta
hcFieldName = typeElement _FieldName stringType

hcFieldType :: Element Meta
hcFieldType = typeElement _FieldType $ TypeRecord [
  FieldType _FieldType_name $ TypeNominal _FieldName,
  FieldType _FieldType_type $ TypeNominal _Type]

hcFloatType :: Element Meta
hcFloatType = typeElement _FloatType $ enum [
  _FloatType_bigfloat,
  _FloatType_float32,
  _FloatType_float64]

hcFloatValue :: Element Meta
hcFloatValue = typeElement _FloatValue $ TypeUnion [
  FieldType _FloatValue_bigfloat bigfloatType,
  FieldType _FloatValue_float32 float32Type,
  FieldType _FloatValue_float64 float64Type]

hcFloatVariant :: Element Meta
hcFloatVariant = typeElement _FloatVariant $ enum [
  _FloatVariant_bigfloat,
  _FloatVariant_float32,
  _FloatVariant_float64]

hcFunction :: Element Meta
hcFunction = typeElement _Function $ TypeUnion [
  FieldType _Function_cases $ TypeList $ TypeNominal _Field,
  FieldType _Function_compareTo $ TypeNominal _Term,
  FieldType _Function_data unitType,
  FieldType _Function_lambda $ TypeNominal _Lambda,
  FieldType _Function_primitive $ TypeNominal _Name,
  FieldType _Function_projection $ TypeNominal _FieldType]

hcFunctionType :: Element Meta
hcFunctionType = typeElement _FunctionType $ TypeRecord [
  FieldType _FunctionType_domain $ TypeNominal _Type,
  FieldType _FunctionType_codomain $ TypeNominal _Type]

hcFunctionVariant :: Element Meta
hcFunctionVariant = typeElement _FunctionVariant $ enum [
  _FunctionVariant_cases,
  _FunctionVariant_compareTo,
  _FunctionVariant_data,
  _FunctionVariant_lambda,
  _FunctionVariant_primitive,
  _FunctionVariant_projection]

hcIntegerType :: Element Meta
hcIntegerType = typeElement _IntegerType $ enum [
  _IntegerType_bigint,
  _IntegerType_int8,
  _IntegerType_int16,
  _IntegerType_int32,
  _IntegerType_int64,
  _IntegerType_uint8,
  _IntegerType_uint16,
  _IntegerType_uint32,
  _IntegerType_uint64]

hcIntegerValue :: Element Meta
hcIntegerValue = typeElement _IntegerValue $ TypeUnion [
  FieldType _IntegerValue_bigint bigintType,
  FieldType _IntegerValue_int8 int8Type,
  FieldType _IntegerValue_int16 int16Type,
  FieldType _IntegerValue_int32 int32Type,
  FieldType _IntegerValue_int64 int64Type,
  FieldType _IntegerValue_uint8 uint8Type,
  FieldType _IntegerValue_uint16 uint16Type,
  FieldType _IntegerValue_uint32 uint32Type,
  FieldType _IntegerValue_uint64 uint64Type]

hcIntegerVariant :: Element Meta
hcIntegerVariant = typeElement _IntegerVariant $ enum [
  _IntegerVariant_bigint,
  _IntegerVariant_int8,
  _IntegerVariant_int16,
  _IntegerVariant_int32,
  _IntegerVariant_int64,
  _IntegerVariant_uint8,
  _IntegerVariant_uint16,
  _IntegerVariant_uint32,
  _IntegerVariant_uint64]

hcLambda :: Element Meta
hcLambda = typeElement _Lambda $ TypeRecord [
  FieldType _Lambda_parameter $ TypeNominal _Variable,
  FieldType _Lambda_body $ TypeNominal _Term]

hcLiteral :: Element Meta
hcLiteral = typeElement _Literal $ TypeUnion [
  FieldType _Literal_binary binaryType,
  FieldType _Literal_boolean $ TypeNominal _BooleanValue,
  FieldType _Literal_float $ TypeNominal _FloatValue,
  FieldType _Literal_integer $ TypeNominal _IntegerValue,
  FieldType _Literal_string stringType]

hcLiteralType :: Element Meta
hcLiteralType = typeElement _LiteralType $ TypeUnion [
  FieldType _LiteralType_binary unitType,
  FieldType _LiteralType_boolean unitType,
  FieldType _LiteralType_float $ TypeNominal _FloatType,
  FieldType _LiteralType_integer $ TypeNominal _IntegerType,
  FieldType _LiteralType_string unitType]

hcLiteralVariant :: Element Meta
hcLiteralVariant = typeElement _LiteralVariant $ enum [
  _LiteralVariant_binary,
  _LiteralVariant_boolean,
  _LiteralVariant_float,
  _LiteralVariant_integer,
  _LiteralVariant_string]

hcMapType :: Element Meta
hcMapType = typeElement _MapType $ TypeRecord [
  FieldType _MapType_keys $ TypeNominal _Type,
  FieldType _MapType_values $ TypeNominal _Type]

hcName :: Element Meta
hcName = typeElement _Name stringType

hcPrecision :: Element Meta
hcPrecision = typeElement _Precision $ TypeUnion [
  FieldType _Precision_arbitrary unitType,
  FieldType _Precision_bits int32Type]

hcTerm :: Element Meta
hcTerm = typeElement _Term $ TypeUnion [
  FieldType _Expression_application $ TypeNominal _Application,
  FieldType _Expression_literal $ TypeNominal _Literal,
  FieldType _Expression_element $ TypeNominal _Name,
  FieldType _Expression_function $ TypeNominal _Function,
  FieldType _Expression_list $ TypeList $ TypeNominal _Term,
  FieldType _Expression_map $ TypeMap $ MapType (TypeNominal _Term) (TypeNominal _Term),
  FieldType _Expression_optional $ TypeOptional $ TypeNominal _Term,
  FieldType _Expression_record $ TypeList $ TypeNominal _Field,
  FieldType _Expression_set $ TypeSet $ TypeNominal _Term,
  FieldType _Expression_union $ TypeNominal _Field,
  FieldType _Expression_variable $ TypeNominal _Variable]

hcTermVariant :: Element Meta
hcTermVariant = typeElement _TermVariant $ enum [
  _TermVariant_application,
  _TermVariant_literal,
  _TermVariant_element,
  _TermVariant_function,
  _TermVariant_list,
  _TermVariant_map,
  _TermVariant_optional,
  _TermVariant_record,
  _TermVariant_set,
  _TermVariant_union,
  _TermVariant_variable]

hcType :: Element Meta
hcType = typeElement _Type $ TypeUnion [
  FieldType _Type_literal $ TypeNominal _LiteralType,
  FieldType _Type_element $ TypeNominal _Type,
  FieldType _Type_function $ TypeNominal _FunctionType,
  FieldType _Type_list $ TypeNominal _Type,
  FieldType _Type_map $ TypeNominal _MapType,
  FieldType _Type_nominal $ TypeNominal _Name,
  FieldType _Type_optional $ TypeNominal _Type,
  FieldType _Type_record $ TypeList $ TypeNominal _FieldType,
  FieldType _Type_set $ TypeNominal _Type,
  FieldType _Type_union $ TypeList $ TypeNominal _FieldType]

hcTypeVariable :: Element Meta
hcTypeVariable = typeElement _TypeVariable stringType

hcTypeVariant :: Element Meta
hcTypeVariant = typeElement _TypeVariant $ enum [
  _TypeVariant_literal,
  _TypeVariant_element,
  _TypeVariant_function,
  _TypeVariant_list,
  _TypeVariant_map,
  _TypeVariant_nominal,
  _TypeVariant_optional,
  _TypeVariant_record,
  _TypeVariant_set,
  _TypeVariant_union]

hcTypedTerm :: Element Meta
hcTypedTerm = typeElement _TypedTerm $ TypeRecord [
  FieldType _TypedTerm_type $ TypeNominal _Type,
  FieldType _TypedTerm_term $ TypeNominal _Term]

hcUniversalType :: Element Meta
hcUniversalType = typeElement _UniversalType $ TypeRecord [
  FieldType _UniversalType_variable stringType,
  FieldType _UniversalType_body $ TypeNominal _Type]

hcVariable :: Element Meta
hcVariable = typeElement _Variable stringType
