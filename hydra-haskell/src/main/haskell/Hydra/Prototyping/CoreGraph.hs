module Hydra.Prototyping.CoreGraph ( hydraCoreGraph ) where

import Hydra.Core
import Hydra.Graph
import Hydra.Prototyping.CoreEncoding
import Hydra.Impl.Haskell.Dsl


-- Note: here, the element namespace "hydra/core" doubles as a graph name
hydraCoreGraph :: Graph
hydraCoreGraph = Graph "hydra/core" elements (const True) "hydra/core"
  where
    elements = [
      hsAbstractType,
      hcApplication,
      hcAtomicType,
      hcAtomicVariant,
      hcAtomicValue,
      hcBooleanValue,
      hcComparison,
      hcField,
      hcFieldName,
      hcFieldType,
      hcFloatType,
      hcFloatValue,
      hcFloatVariant,
      hcFunction,
      hsFunctionType,
      hsFunctionVariant,
      hsIntegerType,
      hsIntegerValue,
      hsIntegerVariant,
      hsLambda,
      hsMapType,
      hsName,
      hsPrecision,
      hsTerm,
      hsTermVariant,
      hsType,
      hsTypeVariable,
      hsTypeVariant,
      hsTypedTerm,
      hsVariable]

typeElement :: Name -> Type -> Element
typeElement name typ = Element {
  elementName = name,
  elementSchema = TermElement _Type,
  elementData = encodeType typ}

enum :: [FieldName] -> Type
enum names = TypeUnion $ (`FieldType` unitType) <$> names

hsAbstractType :: Element
hsAbstractType = typeElement _AbstractType $ TypeRecord [
  FieldType _AbstractType_variable stringType,
  FieldType _AbstractType_body $ TypeNominal _Type]

hcApplication :: Element
hcApplication = typeElement _Application $ TypeRecord [
  FieldType _Application_function $ TypeNominal _Term,
  FieldType _Application_argument $ TypeNominal _Term]

hcAtomicType :: Element
hcAtomicType = typeElement _AtomicType $ TypeUnion [
  FieldType _AtomicType_binary unitType,
  FieldType _AtomicType_boolean unitType,
  FieldType _AtomicType_float $ TypeNominal _FloatType,
  FieldType _AtomicType_integer $ TypeNominal _IntegerType,
  FieldType _AtomicType_string unitType]

hcAtomicVariant :: Element
hcAtomicVariant = typeElement _AtomicVariant $ enum [
  _AtomicVariant_binary,
  _AtomicVariant_boolean,
  _AtomicVariant_float,
  _AtomicVariant_integer,
  _AtomicVariant_string]

hcAtomicValue :: Element
hcAtomicValue = typeElement _AtomicValue $ TypeUnion [
  FieldType _AtomicValue_binary binaryType,
  FieldType _AtomicValue_boolean $ TypeNominal _BooleanValue,
  FieldType _AtomicValue_float $ TypeNominal _FloatValue,
  FieldType _AtomicValue_integer $ TypeNominal _IntegerValue,
  FieldType _AtomicValue_string stringType]

hcBooleanValue :: Element
hcBooleanValue = typeElement _BooleanValue $ enum [
  _BooleanValue_false,
  _BooleanValue_true]

hcComparison :: Element
hcComparison = typeElement _Comparison $ enum [
  _Comparison_lessThan,
  _Comparison_equalTo,
  _Comparison_greaterThan]

hcField :: Element
hcField = typeElement _Field $ TypeRecord [
  FieldType _Field_name $ TypeNominal _FieldName,
  FieldType _Field_term $ TypeNominal _Term]

hcFieldName :: Element
hcFieldName = typeElement _FieldName stringType

hcFieldType :: Element
hcFieldType = typeElement _FieldType $ TypeRecord [
  FieldType _FieldType_name $ TypeNominal _FieldName,
  FieldType _FieldType_type $ TypeNominal _Type]

hcFloatType :: Element
hcFloatType = typeElement _FloatType $ enum [
  _FloatType_bigfloat,
  _FloatType_float32,
  _FloatType_float64]

hcFloatValue :: Element
hcFloatValue = typeElement _FloatValue $ TypeUnion [
  FieldType _FloatValue_bigfloat bigfloatType,
  FieldType _FloatValue_float32 float32Type,
  FieldType _FloatValue_float64 float64Type]

hcFloatVariant :: Element
hcFloatVariant = typeElement _FloatVariant $ enum [
  _FloatVariant_bigfloat,
  _FloatVariant_float32,
  _FloatVariant_float64]

hcFunction :: Element
hcFunction = typeElement _Function $ TypeUnion [
  FieldType _Function_cases $ TypeList $ TypeNominal _Field,
  FieldType _Function_compareTo $ TypeNominal _Term,
  FieldType _Function_data unitType,
  FieldType _Function_lambda $ TypeNominal _Lambda,
  FieldType _Function_primitive $ TypeNominal _Name,
  FieldType _Function_projection $ TypeNominal _FieldType]

hsFunctionType :: Element
hsFunctionType = typeElement _FunctionType $ TypeRecord [
  FieldType _FunctionType_domain $ TypeNominal _Type,
  FieldType _FunctionType_codomain $ TypeNominal _Type]

hsFunctionVariant :: Element
hsFunctionVariant = typeElement _FunctionVariant $ enum [
  _FunctionVariant_cases,
  _FunctionVariant_compareTo,
  _FunctionVariant_data,
  _FunctionVariant_lambda,
  _FunctionVariant_primitive,
  _FunctionVariant_projection]

hsIntegerType :: Element
hsIntegerType = typeElement _IntegerType $ enum [
  _IntegerType_bigint,
  _IntegerType_int8,
  _IntegerType_int16,
  _IntegerType_int32,
  _IntegerType_int64,
  _IntegerType_uint8,
  _IntegerType_uint16,
  _IntegerType_uint32,
  _IntegerType_uint64]

hsIntegerValue :: Element
hsIntegerValue = typeElement _IntegerValue $ TypeUnion [
  FieldType _IntegerValue_bigint bigintType,
  FieldType _IntegerValue_int8 int8Type,
  FieldType _IntegerValue_int16 int16Type,
  FieldType _IntegerValue_int32 int32Type,
  FieldType _IntegerValue_int64 int64Type,
  FieldType _IntegerValue_uint8 uint8Type,
  FieldType _IntegerValue_uint16 uint16Type,
  FieldType _IntegerValue_uint32 uint32Type,
  FieldType _IntegerValue_uint64 uint64Type]

hsIntegerVariant :: Element
hsIntegerVariant = typeElement _IntegerVariant $ enum [
  _IntegerVariant_bigint,
  _IntegerVariant_int8,
  _IntegerVariant_int16,
  _IntegerVariant_int32,
  _IntegerVariant_int64,
  _IntegerVariant_uint8,
  _IntegerVariant_uint16,
  _IntegerVariant_uint32,
  _IntegerVariant_uint64]

hsLambda :: Element
hsLambda = typeElement _Lambda $ TypeRecord [
  FieldType _Lambda_parameter $ TypeNominal _Variable,
  FieldType _Lambda_body $ TypeNominal _Term]

hsMapType :: Element
hsMapType = typeElement _MapType $ TypeRecord [
  FieldType _MapType_keys $ TypeNominal _Type,
  FieldType _MapType_values $ TypeNominal _Type]

hsName :: Element
hsName = typeElement _Name stringType

hsPrecision :: Element
hsPrecision = typeElement _Precision $ TypeUnion [
  FieldType _Precision_arbitrary unitType,
  FieldType _Precision_bits int32Type]

hsTerm :: Element
hsTerm = typeElement _Term $ TypeUnion [
  FieldType _Term_application $ TypeNominal _Application,
  FieldType _Term_atomic $ TypeNominal _AtomicValue,
  FieldType _Term_element $ TypeNominal _Name,
  FieldType _Term_function $ TypeNominal _Function,
  FieldType _Term_list $ TypeList $ TypeNominal _Term,
  FieldType _Term_map $ TypeMap $ MapType (TypeNominal _Term) (TypeNominal _Term),
  FieldType _Term_optional $ TypeOptional $ TypeNominal _Term,
  FieldType _Term_record $ TypeList $ TypeNominal _Field,
  FieldType _Term_set $ TypeSet $ TypeNominal _Term,
  FieldType _Term_union $ TypeNominal _Field,
  FieldType _Term_variable $ TypeNominal _Variable]

hsTermVariant :: Element
hsTermVariant = typeElement _TermVariant $ enum [
  _TermVariant_application,
  _TermVariant_atomic,
  _TermVariant_element,
  _TermVariant_function,
  _TermVariant_list,
  _TermVariant_map,
  _TermVariant_optional,
  _TermVariant_record,
  _TermVariant_set,
  _TermVariant_union,
  _TermVariant_variable]

hsType :: Element
hsType = typeElement _Type $ TypeUnion [
  FieldType _Type_atomic $ TypeNominal _AtomicType,
  FieldType _Type_element $ TypeNominal _Type,
  FieldType _Type_function $ TypeNominal _FunctionType,
  FieldType _Type_list $ TypeNominal _Type,
  FieldType _Type_map $ TypeNominal _MapType,
  FieldType _Type_nominal $ TypeNominal _Name,
  FieldType _Type_optional $ TypeNominal _Type,
  FieldType _Type_record $ TypeList $ TypeNominal _FieldType,
  FieldType _Type_set $ TypeNominal _Type,
  FieldType _Type_union $ TypeList $ TypeNominal _FieldType]

hsTypeVariable :: Element
hsTypeVariable = typeElement _TypeVariable stringType

hsTypeVariant :: Element
hsTypeVariant = typeElement _TypeVariant $ enum [
  _TypeVariant_atomic,
  _TypeVariant_element,
  _TypeVariant_function,
  _TypeVariant_list,
  _TypeVariant_map,
  _TypeVariant_nominal,
  _TypeVariant_optional,
  _TypeVariant_record,
  _TypeVariant_set,
  _TypeVariant_union]

hsTypedTerm :: Element
hsTypedTerm = typeElement _TypedTerm $ TypeRecord [
  FieldType _TypedTerm_type $ TypeNominal _Type,
  FieldType _TypedTerm_term $ TypeNominal _Term]

hsVariable :: Element
hsVariable = typeElement _Variable stringType
