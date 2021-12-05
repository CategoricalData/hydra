module Hydra.Prototyping.CoreGraph (
  emptyCoreContext,
  hydraCoreGraph,

  hcApplication,
  hcBooleanValue,
  hcComparison,
  hcExpression,
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
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.CoreEncoding
import Hydra.Impl.Haskell.Dsl.Terms

import qualified Data.Map as M
import qualified Data.Set as S


emptyCoreContext :: Context Meta
emptyCoreContext = Context {
    contextGraphs = GraphSet {
      graphSetGraphs = M.fromList [(emptyGraphName, emptyGraph)],
      graphSetRoot = emptyGraphName},
    contextElements = M.empty,
    contextFunctions = M.empty,
    contextStrategy = EvaluationStrategy {
      evaluationStrategyOpaqueTermVariants = S.fromList []},
    contextDescriptionOf = metaDescription,
    contextTypeOf = metaType,
    contextSetTypeOf = \t m -> m {metaType = t}}
  where
    emptyGraphName = "empty"
    emptyGraph = Graph emptyGraphName [] (const True) "empty"

-- Note: here, the element namespace "hydra/core" doubles as a graph name
hydraCoreGraph :: Graph Meta
hydraCoreGraph = Graph "hydra/core" elements (const True) "hydra/core"
  where
    elements = (\f -> f emptyCoreContext) <$> [
      hcApplication,
      hcBooleanValue,
      hcComparison,
      hcExpression,
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

typeElement :: Context Meta -> Name -> Type -> Element Meta
typeElement cx name typ = Element {
  elementName = name,
  elementSchema = defaultTerm $ ExpressionElement _Type,
  elementData = encodeType cx typ}

enum :: [FieldName] -> Type
enum names = TypeUnion $ (`FieldType` unitType) <$> names

hcApplication :: Context Meta -> Element Meta
hcApplication cx = typeElement cx _Application $ TypeRecord [
  FieldType _Application_function $ TypeNominal _Term,
  FieldType _Application_argument $ TypeNominal _Term]

hcBooleanValue :: Context Meta -> Element Meta
hcBooleanValue cx = typeElement cx _BooleanValue $ enum [
  _BooleanValue_false,
  _BooleanValue_true]

hcComparison :: Context Meta -> Element Meta
hcComparison cx = typeElement cx _Comparison $ enum [
  _Comparison_lessThan,
  _Comparison_equalTo,
  _Comparison_greaterThan]

hcExpression :: Context Meta -> Element Meta
hcExpression cx = typeElement cx _Expression $ TypeUnion [
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

hcField :: Context Meta -> Element Meta
hcField cx = typeElement cx _Field $ TypeRecord [
  FieldType _Field_name $ TypeNominal _FieldName,
  FieldType _Field_term $ TypeNominal _Term]

hcFieldName :: Context Meta -> Element Meta
hcFieldName cx = typeElement cx _FieldName stringType

hcFieldType :: Context Meta -> Element Meta
hcFieldType cx = typeElement cx _FieldType $ TypeRecord [
  FieldType _FieldType_name $ TypeNominal _FieldName,
  FieldType _FieldType_type $ TypeNominal _Type]

hcFloatType :: Context Meta -> Element Meta
hcFloatType cx = typeElement cx _FloatType $ enum [
  _FloatType_bigfloat,
  _FloatType_float32,
  _FloatType_float64]

hcFloatValue :: Context Meta -> Element Meta
hcFloatValue cx = typeElement cx _FloatValue $ TypeUnion [
  FieldType _FloatValue_bigfloat bigfloatType,
  FieldType _FloatValue_float32 float32Type,
  FieldType _FloatValue_float64 float64Type]

hcFloatVariant :: Context Meta -> Element Meta
hcFloatVariant cx = typeElement cx _FloatVariant $ enum [
  _FloatVariant_bigfloat,
  _FloatVariant_float32,
  _FloatVariant_float64]

hcFunction :: Context Meta -> Element Meta
hcFunction cx = typeElement cx _Function $ TypeUnion [
  FieldType _Function_cases $ TypeList $ TypeNominal _Field,
  FieldType _Function_compareTo $ TypeNominal _Term,
  FieldType _Function_data unitType,
  FieldType _Function_lambda $ TypeNominal _Lambda,
  FieldType _Function_primitive $ TypeNominal _Name,
  FieldType _Function_projection $ TypeNominal _FieldType]

hcFunctionType :: Context Meta -> Element Meta
hcFunctionType cx = typeElement cx _FunctionType $ TypeRecord [
  FieldType _FunctionType_domain $ TypeNominal _Type,
  FieldType _FunctionType_codomain $ TypeNominal _Type]

hcFunctionVariant :: Context Meta -> Element Meta
hcFunctionVariant cx = typeElement cx _FunctionVariant $ enum [
  _FunctionVariant_cases,
  _FunctionVariant_compareTo,
  _FunctionVariant_data,
  _FunctionVariant_lambda,
  _FunctionVariant_primitive,
  _FunctionVariant_projection]

hcIntegerType :: Context Meta -> Element Meta
hcIntegerType cx = typeElement cx _IntegerType $ enum [
  _IntegerType_bigint,
  _IntegerType_int8,
  _IntegerType_int16,
  _IntegerType_int32,
  _IntegerType_int64,
  _IntegerType_uint8,
  _IntegerType_uint16,
  _IntegerType_uint32,
  _IntegerType_uint64]

hcIntegerValue :: Context Meta -> Element Meta
hcIntegerValue cx = typeElement cx _IntegerValue $ TypeUnion [
  FieldType _IntegerValue_bigint bigintType,
  FieldType _IntegerValue_int8 int8Type,
  FieldType _IntegerValue_int16 int16Type,
  FieldType _IntegerValue_int32 int32Type,
  FieldType _IntegerValue_int64 int64Type,
  FieldType _IntegerValue_uint8 uint8Type,
  FieldType _IntegerValue_uint16 uint16Type,
  FieldType _IntegerValue_uint32 uint32Type,
  FieldType _IntegerValue_uint64 uint64Type]

hcIntegerVariant :: Context Meta -> Element Meta
hcIntegerVariant cx = typeElement cx _IntegerVariant $ enum [
  _IntegerVariant_bigint,
  _IntegerVariant_int8,
  _IntegerVariant_int16,
  _IntegerVariant_int32,
  _IntegerVariant_int64,
  _IntegerVariant_uint8,
  _IntegerVariant_uint16,
  _IntegerVariant_uint32,
  _IntegerVariant_uint64]

hcLambda :: Context Meta -> Element Meta
hcLambda cx = typeElement cx _Lambda $ TypeRecord [
  FieldType _Lambda_parameter $ TypeNominal _Variable,
  FieldType _Lambda_body $ TypeNominal _Term]

hcLiteral :: Context Meta -> Element Meta
hcLiteral cx = typeElement cx _Literal $ TypeUnion [
  FieldType _Literal_binary binaryType,
  FieldType _Literal_boolean $ TypeNominal _BooleanValue,
  FieldType _Literal_float $ TypeNominal _FloatValue,
  FieldType _Literal_integer $ TypeNominal _IntegerValue,
  FieldType _Literal_string stringType]

hcLiteralType :: Context Meta -> Element Meta
hcLiteralType cx = typeElement cx _LiteralType $ TypeUnion [
  FieldType _LiteralType_binary unitType,
  FieldType _LiteralType_boolean unitType,
  FieldType _LiteralType_float $ TypeNominal _FloatType,
  FieldType _LiteralType_integer $ TypeNominal _IntegerType,
  FieldType _LiteralType_string unitType]

hcLiteralVariant :: Context Meta -> Element Meta
hcLiteralVariant cx = typeElement cx _LiteralVariant $ enum [
  _LiteralVariant_binary,
  _LiteralVariant_boolean,
  _LiteralVariant_float,
  _LiteralVariant_integer,
  _LiteralVariant_string]

hcMapType :: Context Meta -> Element Meta
hcMapType cx = typeElement cx _MapType $ TypeRecord [
  FieldType _MapType_keys $ TypeNominal _Type,
  FieldType _MapType_values $ TypeNominal _Type]

hcName :: Context Meta -> Element Meta
hcName cx = typeElement cx _Name stringType

hcPrecision :: Context Meta -> Element Meta
hcPrecision cx = typeElement cx _Precision $ TypeUnion [
  FieldType _Precision_arbitrary unitType,
  FieldType _Precision_bits int32Type]

hcTerm :: Context Meta -> Element Meta
hcTerm cx = typeElement cx _Term $ TypeRecord [
  FieldType _Term_data $ TypeNominal _Expression,
  FieldType _Term_meta unitType] -- TODO: encoding for termMeta

hcTermVariant :: Context Meta -> Element Meta
hcTermVariant cx = typeElement cx _TermVariant $ enum [
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

hcType :: Context Meta -> Element Meta
hcType cx = typeElement cx _Type $ TypeUnion [
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

hcTypeVariable :: Context Meta -> Element Meta
hcTypeVariable cx = typeElement cx _TypeVariable stringType

hcTypeVariant :: Context Meta -> Element Meta
hcTypeVariant cx = typeElement cx _TypeVariant $ enum [
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

hcTypedTerm :: Context Meta -> Element Meta
hcTypedTerm cx = typeElement cx _TypedTerm $ TypeRecord [
  FieldType _TypedTerm_type $ TypeNominal _Type,
  FieldType _TypedTerm_term $ TypeNominal _Term]

hcUniversalType :: Context Meta -> Element Meta
hcUniversalType cx = typeElement cx _UniversalType $ TypeRecord [
  FieldType _UniversalType_variable stringType,
  FieldType _UniversalType_body $ TypeNominal _Type]

hcVariable :: Context Meta -> Element Meta
hcVariable cx = typeElement cx _Variable stringType
