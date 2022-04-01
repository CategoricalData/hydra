module Hydra.Impl.Haskell.Sources.CoreGraph where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Dsl.Standard


hydraCoreGraph :: Graph Meta
hydraCoreGraph = Graph modName elements (const True) "hydra/core"
  where
    -- Note: here, the element namespace "hydra/core" doubles as a graph name
    modName = "hydra/core"

    element lname doc typ = typeElement standardContext (modName ++ "." ++ lname) doc typ

    elements = [

      element "Application"
        "A term which applies a function to an argument" $
        universal "m" $ TypeRecord [
          FieldType _Application_function $ universal "m" $ TypeNominal _Term,
          FieldType _Application_argument $ universal "m" $ TypeNominal _Term],

      element "BooleanValue"
        "A boolean literal value" $
        enum [
          _BooleanValue_false,
          _BooleanValue_true],

      element "Comparison"
        "An equality judgement: less than, equal to, or greater than" $
        enum [
          _Comparison_lessThan,
          _Comparison_equalTo,
          _Comparison_greaterThan],

      element "Expression"
        "A term expression" $
        universal "m" $ TypeUnion [
          FieldType _Expression_application $ universal "m" $ TypeNominal _Application,
          FieldType _Expression_literal $ TypeNominal _Literal,
          FieldType _Expression_element $ TypeNominal _Name,
          FieldType _Expression_function $ universal "m" $ TypeNominal _Function,
          FieldType _Expression_list $ TypeList $ universal "m" $ TypeNominal _Term,
          FieldType _Expression_map $ TypeMap $ MapType (universal "m" $ TypeNominal _Term) (universal "m" $ TypeNominal _Term),
          FieldType _Expression_nominal $ universal "m" $ TypeNominal _NominalTerm,
          FieldType _Expression_optional $ TypeOptional $ universal "m" $ TypeNominal _Term,
          FieldType _Expression_record $ TypeList $ universal "m" $ TypeNominal _Field,
          FieldType _Expression_set $ TypeSet $ universal "m" $ TypeNominal _Term,
          FieldType _Expression_typeAbstraction $ universal "m" $ TypeNominal _TypeAbstraction,
          FieldType _Expression_typeApplication $ universal "m" $ TypeNominal _TypeApplication,
          FieldType _Expression_union $ universal "m" $ TypeNominal _Field,
          FieldType _Expression_variable $ TypeNominal _Variable],

      element "Field"
        "A labeled term" $
        universal "m" $ TypeRecord [
          FieldType _Field_name $ TypeNominal _FieldName,
          FieldType _Field_term $ universal "m" $ TypeNominal _Term],

      element "FieldName"
        "The name of a field"
        stringType,

      element "FieldType"
        "The name and type of a field" $
        TypeRecord [
          FieldType _FieldType_name $ TypeNominal _FieldName,
          FieldType _FieldType_type $ TypeNominal _Type],

      element "FloatType"
        "A floating-point type" $
        enum [
          _FloatType_bigfloat,
          _FloatType_float32,
          _FloatType_float64],

      element "FloatValue"
        "A floating-point literal value" $
        TypeUnion [
          FieldType _FloatValue_bigfloat bigfloatType,
          FieldType _FloatValue_float32 float32Type,
          FieldType _FloatValue_float64 float64Type],

      element "Function"
        "A function" $
        universal "m" $ TypeUnion [
          FieldType _Function_cases $ TypeList $ universal "m" $ TypeNominal _Field,
          FieldType _Function_compareTo $ universal "m" $ TypeNominal _Term,
          FieldType _Function_data unitType,
          FieldType _Function_lambda $ universal "m" $ TypeNominal _Lambda,
          FieldType _Function_optionalCases $ universal "m" $ TypeNominal _OptionalCases,
          FieldType _Function_primitive $ TypeNominal _Name,
          FieldType _Function_projection $ TypeNominal _FieldName],

      element "FunctionType"
        "A function type, also known as an arrow type" $
        TypeRecord [
          FieldType _FunctionType_domain $ TypeNominal _Type,
          FieldType _FunctionType_codomain $ TypeNominal _Type],

      element "FunctionVariant"
        "The identifier of a function constructor" $
        enum [
          _FunctionVariant_cases,
          _FunctionVariant_compareTo,
          _FunctionVariant_data,
          _FunctionVariant_lambda,
          _FunctionVariant_optionalCases,
          _FunctionVariant_primitive,
          _FunctionVariant_projection],

      element "IntegerType"
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
          _IntegerType_uint64],

      element "IntegerValue"
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
          FieldType _IntegerValue_uint64 uint64Type],

      element "Lambda"
        "A function abstraction (lambda)" $
        universal "m" $ TypeRecord [
          FieldType _Lambda_parameter $ TypeNominal _Variable,
          FieldType _Lambda_body $ universal "m" $ TypeNominal _Term],

      element "Let"
        "A 'let' binding" $
        universal "m" $ TypeRecord [
          FieldType _Let_key $ TypeNominal _Variable,
          FieldType _Let_value $ universal "m" $ TypeNominal _Term,
          FieldType _Let_environment $ universal "m" $ TypeNominal _Term],

      element "Literal"
        "A term constant; an instance of a literal type" $
        TypeUnion [
          FieldType _Literal_binary binaryType,
          FieldType _Literal_boolean $ TypeNominal _BooleanValue,
          FieldType _Literal_float $ TypeNominal _FloatValue,
          FieldType _Literal_integer $ TypeNominal _IntegerValue,
          FieldType _Literal_string stringType],

      element "LiteralType"
        "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants" $
        TypeUnion [
          FieldType _LiteralType_binary unitType,
          FieldType _LiteralType_boolean unitType,
          FieldType _LiteralType_float $ TypeNominal _FloatType,
          FieldType _LiteralType_integer $ TypeNominal _IntegerType,
          FieldType _LiteralType_string unitType],

      element "LiteralVariant"
        "The identifier of a literal constructor" $
        enum [
          _LiteralVariant_binary,
          _LiteralVariant_boolean,
          _LiteralVariant_float,
          _LiteralVariant_integer,
          _LiteralVariant_string],

      element "MapType"
        "A map type" $
        TypeRecord [
          FieldType _MapType_keys $ TypeNominal _Type,
          FieldType _MapType_values $ TypeNominal _Type],

      element "Meta"
        "A built-in metadata container for terms" $
        TypeRecord [
          FieldType _Meta_description (TypeOptional stringType),
          FieldType _Meta_type (TypeOptional $ TypeNominal _Type)],

      element "Name"
        "A unique element name"
        stringType,

      element "NominalTerm"
        "A term annotated with a fixed, named type; an instance of a newtype" $
        universal "m" $ TypeRecord [
          FieldType _NominalTerm_typeName (TypeNominal _Name),
          FieldType _NominalTerm_term (universal "m" $ TypeNominal _Term)],

      element "OptionalCases"
        "A case statement for matching optional terms" $
        universal "m" $ TypeRecord [
          FieldType _OptionalCases_nothing (universal "m" $ TypeNominal _Term),
          FieldType _OptionalCases_just (universal "m" $ TypeNominal _Term)],

      element "OptionalExpression"
        "An encoded optional value, for languages which do not natively support optionals" $
        universal "m" $ TypeUnion [
          FieldType _OptionalExpression_just (universal "m" $ TypeNominal _Term),
          FieldType _OptionalExpression_nothing unitType],

      element "Precision"
        "Numeric precision: arbitrary precision, or precision to a specified number of bits" $
        TypeUnion [
          FieldType _Precision_arbitrary unitType,
          FieldType _Precision_bits int32Type],

      element "Term"
        "A data term" $
        universal "m" $ TypeRecord [
          FieldType _Term_data $ universal "m" $ TypeNominal _Expression,
          FieldType _Term_meta $ typeVariable "m"],

      element "TermVariant"
        "The identifier of a term expression constructor" $
        enum [
          _TermVariant_application,
          _TermVariant_element,
          _TermVariant_function,
          _TermVariant_let,
          _TermVariant_list,
          _TermVariant_literal,
          _TermVariant_map,
          _TermVariant_nominal,
          _TermVariant_optional,
          _TermVariant_record,
          _TermVariant_set,
          _TermVariant_typeAbstraction,
          _TermVariant_typeApplication,
          _TermVariant_union,
          _TermVariant_variable],

      element "Type"
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
          FieldType _Type_variable $ TypeNominal _TypeVariable],

      element "TypeAbstraction"
        "A type abstraction (generalization), which binds a type variable to a term" $
        universal "m" $ TypeRecord [
          FieldType _TypeAbstraction_parameter (TypeNominal _TypeVariable),
          FieldType _TypeAbstraction_body (universal "m" $ TypeNominal _Term)],

      element "TypeApplication"
        "A type application (instantiation), which applies a term to a type" $
        universal "m" $ TypeRecord [
          FieldType _TypeApplication_function (universal "m" $ TypeNominal _Term),
          FieldType _TypeApplication_argument (TypeNominal _Type)],

      element "TypeScheme"
        "A type expression together with free type variables occurring in the expression" $
        TypeRecord [
          FieldType _TypeScheme_variables (TypeList $ TypeNominal _TypeVariable),
          FieldType _TypeScheme_type (TypeNominal _Type)],

      element "TypeVariable"
        "A symbol which stands in for a type"
        stringType,

      element "TypeVariant"
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
          _TypeVariant_union],

      element "TypedTerm"
        "A type together with an instance of the type" $
        universal "m" $ TypeRecord [
          FieldType _TypedTerm_type $ TypeNominal _Type,
          FieldType _TypedTerm_term $ universal "m" $ TypeNominal _Term],

      element "UniversalType"
        "A universally quantified ('forall') type, parameterized by a type variable" $
        TypeRecord [
          FieldType _UniversalType_variable stringType,
          FieldType _UniversalType_body $ TypeNominal _Type],

      element "Variable"
        "A symbol which stands in for a term"
        stringType]
