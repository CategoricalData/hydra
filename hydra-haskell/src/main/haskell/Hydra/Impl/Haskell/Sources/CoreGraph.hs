module Hydra.Impl.Haskell.Sources.CoreGraph where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Dsl.Standard


-- Note: here, the element namespace "hydra/core" doubles as a graph name
hydraCoreGraph :: Graph Meta
hydraCoreGraph = Graph "hydra/core" elements (const True) "hydra/core"
  where
    elements = (\f -> f standardContext) <$> [
      hcApplication,
      hcBooleanValue,
      hcComparison,
      hcExpression,
      hcField,
      hcFieldName,
      hcFieldType,
      hcFloatType,
      hcFloatValue,
      hcFloatType,
      hcFunction,
      hcFunctionType,
      hcFunctionVariant,
      hcIntegerType,
      hcIntegerValue,
      hcLambda,
      hcLet,
      hcLiteral,
      hcLiteralType,
      hcLiteralVariant,
      hcMapType,
      hcMeta,
      hcName,
      hcNominalTerm,
      hcOptionalCases,
      hcOptionalExpression,
      hcPrecision,
      hcTerm,
      hcTermVariant,
      hcType,
      hcTypeAbstraction,
      hcTypeApplication,
      hcTypeScheme,
      hcTypeVariable,
      hcTypeVariant,
      hcTypedTerm,
      hcUniversalType,
      hcVariable]

hcApplication :: Context Meta -> Element Meta
hcApplication cx = typeElement cx _Application
  "A term which applies a function to an argument" $
  TypeRecord [
    FieldType _Application_function $ universal "a" $ TypeNominal _Term,
    FieldType _Application_argument $ universal "a" $ TypeNominal _Term]

hcBooleanValue :: Context Meta -> Element Meta
hcBooleanValue cx = typeElement cx _BooleanValue
  "A boolean literal value" $
  enum [
    _BooleanValue_false,
    _BooleanValue_true]

hcComparison :: Context Meta -> Element Meta
hcComparison cx = typeElement cx _Comparison
  "An equality judgement: less than, equal to, or greater than" $
  enum [
    _Comparison_lessThan,
    _Comparison_equalTo,
    _Comparison_greaterThan]

hcExpression :: Context Meta -> Element Meta
hcExpression cx = typeElement cx _Expression
  "A term expression" $
  TypeUnion [
    FieldType _Expression_application $ TypeNominal _Application,
    FieldType _Expression_literal $ TypeNominal _Literal,
    FieldType _Expression_element $ TypeNominal _Name,
    FieldType _Expression_function $ TypeNominal _Function,
    FieldType _Expression_list $ TypeList $ TypeNominal _Term,
    FieldType _Expression_map $ TypeMap $ MapType (TypeNominal _Term) (TypeNominal _Term),
    FieldType _Expression_nominal $ TypeNominal _NominalTerm,
    FieldType _Expression_optional $ TypeOptional $ TypeNominal _Term,
    FieldType _Expression_record $ TypeList $ TypeNominal _Field,
    FieldType _Expression_set $ TypeSet $ TypeNominal _Term,
    FieldType _Expression_typeAbstraction $ TypeNominal _TypeAbstraction,
    FieldType _Expression_typeApplication $ TypeNominal _TypeApplication,
    FieldType _Expression_union $ TypeNominal _Field,
    FieldType _Expression_variable $ TypeNominal _Variable]

hcField :: Context Meta -> Element Meta
hcField cx = typeElement cx _Field
  "A labeled term" $
  TypeRecord [
    FieldType _Field_name $ TypeNominal _FieldName,
    FieldType _Field_term $ TypeNominal _Term]

hcFieldName :: Context Meta -> Element Meta
hcFieldName cx = typeElement cx _FieldName
  "The name of a field"
  stringType

hcFieldType :: Context Meta -> Element Meta
hcFieldType cx = typeElement cx _FieldType
  "The name and type of a field" $
  TypeRecord [
    FieldType _FieldType_name $ TypeNominal _FieldName,
    FieldType _FieldType_type $ TypeNominal _Type]

hcFloatType :: Context Meta -> Element Meta
hcFloatType cx = typeElement cx _FloatType
  "A floating-point type" $
  enum [
    _FloatType_bigfloat,
    _FloatType_float32,
    _FloatType_float64]

hcFloatValue :: Context Meta -> Element Meta
hcFloatValue cx = typeElement cx _FloatValue
  "A floating-point literal value" $
  TypeUnion [
    FieldType _FloatValue_bigfloat bigfloatType,
    FieldType _FloatValue_float32 float32Type,
    FieldType _FloatValue_float64 float64Type]

hcFunction :: Context Meta -> Element Meta
hcFunction cx = typeElement cx _Function
  "A function" $
  TypeUnion [
    FieldType _Function_cases $ TypeList $ TypeNominal _Field,
    FieldType _Function_compareTo $ TypeNominal _Term,
    FieldType _Function_data unitType,
    FieldType _Function_lambda $ TypeNominal _Lambda,
    FieldType _Function_optionalCases $ TypeNominal _OptionalCases,
    FieldType _Function_primitive $ TypeNominal _Name,
    FieldType _Function_projection $ TypeNominal _FieldName]

hcFunctionType :: Context Meta -> Element Meta
hcFunctionType cx = typeElement cx _FunctionType
  "A function type, also known as an arrow type" $
  TypeRecord [
    FieldType _FunctionType_domain $ TypeNominal _Type,
    FieldType _FunctionType_codomain $ TypeNominal _Type]

hcFunctionVariant :: Context Meta -> Element Meta
hcFunctionVariant cx = typeElement cx _FunctionVariant
  "The identifier of a function constructor" $
  enum [
    _FunctionVariant_cases,
    _FunctionVariant_compareTo,
    _FunctionVariant_data,
    _FunctionVariant_lambda,
    _FunctionVariant_optionalCases,
    _FunctionVariant_primitive,
    _FunctionVariant_projection]

hcIntegerType :: Context Meta -> Element Meta
hcIntegerType cx = typeElement cx _IntegerType
  "An integer type" $
  enum [
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
hcIntegerValue cx = typeElement cx _IntegerValue
  "An integer literal value" $
  TypeUnion [
    FieldType _IntegerValue_bigint bigintType,
    FieldType _IntegerValue_int8 int8Type,
    FieldType _IntegerValue_int16 int16Type,
    FieldType _IntegerValue_int32 int32Type,
    FieldType _IntegerValue_int64 int64Type,
    FieldType _IntegerValue_uint8 uint8Type,
    FieldType _IntegerValue_uint16 uint16Type,
    FieldType _IntegerValue_uint32 uint32Type,
    FieldType _IntegerValue_uint64 uint64Type]

hcLambda :: Context Meta -> Element Meta
hcLambda cx = typeElement cx _Lambda
  "A function abstraction (lambda)" $
  TypeRecord [
    FieldType _Lambda_parameter $ TypeNominal _Variable,
    FieldType _Lambda_body $ TypeNominal _Term]

hcLet :: Context Meta -> Element Meta
hcLet cx = typeElement cx _Let
  "A 'let' binding" $
  TypeRecord [
    FieldType _Let_key $ TypeNominal _Variable,
    FieldType _Let_value $ TypeNominal _Term,
    FieldType _Let_environment $ TypeNominal _Term]

hcLiteral :: Context Meta -> Element Meta
hcLiteral cx = typeElement cx _Literal
  "A term constant; an instance of a literal type" $
  TypeUnion [
    FieldType _Literal_binary binaryType,
    FieldType _Literal_boolean $ TypeNominal _BooleanValue,
    FieldType _Literal_float $ TypeNominal _FloatValue,
    FieldType _Literal_integer $ TypeNominal _IntegerValue,
    FieldType _Literal_string stringType]

hcLiteralType :: Context Meta -> Element Meta
hcLiteralType cx = typeElement cx _LiteralType
  "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants" $
  TypeUnion [
    FieldType _LiteralType_binary unitType,
    FieldType _LiteralType_boolean unitType,
    FieldType _LiteralType_float $ TypeNominal _FloatType,
    FieldType _LiteralType_integer $ TypeNominal _IntegerType,
    FieldType _LiteralType_string unitType]

hcLiteralVariant :: Context Meta -> Element Meta
hcLiteralVariant cx = typeElement cx _LiteralVariant
  "The identifier of a literal constructor" $
  enum [
    _LiteralVariant_binary,
    _LiteralVariant_boolean,
    _LiteralVariant_float,
    _LiteralVariant_integer,
    _LiteralVariant_string]

hcMapType :: Context Meta -> Element Meta
hcMapType cx = typeElement cx _MapType
  "A map type" $
  TypeRecord [
    FieldType _MapType_keys $ TypeNominal _Type,
    FieldType _MapType_values $ TypeNominal _Type]

hcMeta :: Context Meta -> Element Meta
hcMeta cx = typeElement cx _Meta
  "A built-in metadata container for terms" $
  TypeRecord [
    FieldType _Meta_description (TypeOptional stringType),
    FieldType _Meta_type (TypeOptional $ TypeNominal _Type)]

hcName :: Context Meta -> Element Meta
hcName cx = typeElement cx _Name
  "A unique element name"
  stringType

hcNominalTerm :: Context Meta -> Element Meta
hcNominalTerm cx = typeElement cx _NominalTerm
  "A term annotated with a fixed, named type; an instance of a newtype" $
  TypeRecord [
    FieldType _NominalTerm_typeName (TypeNominal _Name),
    FieldType _NominalTerm_term (TypeNominal _Term)]

hcOptionalCases :: Context Meta -> Element Meta
hcOptionalCases cx = typeElement cx _OptionalCases
  "A case statement for matching optional terms" $
  TypeRecord [
    FieldType _OptionalCases_nothing (TypeNominal _Term),
    FieldType _OptionalCases_just (TypeNominal _Term)]

hcOptionalExpression :: Context Meta -> Element Meta
hcOptionalExpression cx = typeElement cx _OptionalExpression
  "An encoded optional value, for languages which do not natively support optionals" $
  TypeUnion [
    FieldType _OptionalExpression_just (TypeNominal _Term),
    FieldType _OptionalExpression_nothing unitType]

hcPrecision :: Context Meta -> Element Meta
hcPrecision cx = typeElement cx _Precision
  "Numeric precision: arbitrary precision, or precision to a specified number of bits" $
  TypeUnion [
    FieldType _Precision_arbitrary unitType,
    FieldType _Precision_bits int32Type]

hcTerm :: Context Meta -> Element Meta
hcTerm cx = typeElement cx _Term
  "A data term" $
  TypeRecord [
    FieldType _Term_data $ TypeNominal _Expression,
    FieldType _Term_meta unitType] -- TODO: encoding for termMeta

hcTermVariant :: Context Meta -> Element Meta
hcTermVariant cx = typeElement cx _TermVariant
  "The identifier of a term expression constructor" $
  enum [
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
hcType cx = typeElement cx _Type
  "A data type" $
  TypeUnion [
    FieldType _Type_literal $ TypeNominal _LiteralType,
    FieldType _Type_element $ TypeNominal _Type,
    FieldType _Type_function $ TypeNominal _FunctionType,
    FieldType _Type_list $ TypeNominal _Type,
    FieldType _Type_map $ TypeNominal _MapType,
    FieldType _Type_nominal $ TypeNominal _Name,
    FieldType _Type_optional $ TypeNominal _Type,
    FieldType _Type_record $ TypeList $ TypeNominal _FieldType,
    FieldType _Type_set $ TypeNominal _Type,
    FieldType _Type_union $ TypeList $ TypeNominal _FieldType,
    FieldType _Type_universal $ TypeNominal _UniversalType,
    FieldType _Type_variable $ TypeNominal _TypeVariable]

hcTypeAbstraction :: Context Meta -> Element Meta
hcTypeAbstraction cx = typeElement cx _TypeAbstraction
  "A type abstraction (generalization), which binds a type variable to a term" $
  TypeRecord [
    FieldType _TypeAbstraction_parameter (TypeNominal _TypeVariable),
    FieldType _TypeAbstraction_body (TypeNominal _Term)]

hcTypeApplication :: Context Meta -> Element Meta
hcTypeApplication cx = typeElement cx _TypeApplication
  "A type application (instantiation), which applies a term to a type" $
  TypeRecord [
    FieldType _TypeApplication_function (TypeNominal _Term),
    FieldType _TypeApplication_argument (TypeNominal _Type)]

hcTypeScheme :: Context Meta -> Element Meta
hcTypeScheme cx = typeElement cx _TypeScheme
  "A type expression together with free type variables occurring in the expression" $
  TypeRecord [
    FieldType _TypeScheme_variables (TypeList $ TypeNominal _TypeVariable),
    FieldType _TypeScheme_type (TypeNominal _Type)]

hcTypeVariable :: Context Meta -> Element Meta
hcTypeVariable cx = typeElement cx _TypeVariable
  "A symbol which stands in for a type"
  stringType

hcTypeVariant :: Context Meta -> Element Meta
hcTypeVariant cx = typeElement cx _TypeVariant
  "The identifier of a type constructor" $
  enum [
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
hcTypedTerm cx = typeElement cx _TypedTerm
  "A type together with an instance of the type" $
  TypeRecord [
    FieldType _TypedTerm_type $ TypeNominal _Type,
    FieldType _TypedTerm_term $ TypeNominal _Term]

hcUniversalType :: Context Meta -> Element Meta
hcUniversalType cx = typeElement cx _UniversalType
  "A universally quantified ('forall') type, parameterized by a type variable" $
  TypeRecord [
    FieldType _UniversalType_variable stringType,
    FieldType _UniversalType_body $ TypeNominal _Type]

hcVariable :: Context Meta -> Element Meta
hcVariable cx = typeElement cx _Variable
  "A symbol which stands in for a term"
  stringType
