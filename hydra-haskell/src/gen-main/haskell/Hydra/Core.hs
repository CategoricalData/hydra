module Hydra.Core where

import Data.Map
import Data.Set

-- A term which applies a function to an argument
data Application m 
  = Application {
    applicationFunction :: (Term m),
    applicationArgument :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Application = "Application"

_Application_function = "function"

_Application_argument = "argument"

-- A boolean literal value
data BooleanValue 
  = BooleanValueFalse 
  | BooleanValueTrue 
  deriving (Eq, Ord, Read, Show)

_BooleanValue = "BooleanValue"

_BooleanValue_false = "false"

_BooleanValue_true = "true"

-- An equality judgement: less than, equal to, or greater than
data Comparison 
  = ComparisonLessThan 
  | ComparisonEqualTo 
  | ComparisonGreaterThan 
  deriving (Eq, Ord, Read, Show)

_Comparison = "Comparison"

_Comparison_lessThan = "lessThan"

_Comparison_equalTo = "equalTo"

_Comparison_greaterThan = "greaterThan"

-- A term expression
data Expression m 
  = ExpressionApplication (Application m)
  | ExpressionLiteral Literal
  | ExpressionElement Name
  | ExpressionFunction (Function m)
  | ExpressionLet (Let m)
  | ExpressionList [Term m]
  | ExpressionMap (Map (Term m) (Term m))
  | ExpressionNominal (NominalTerm m)
  | ExpressionOptional (Maybe (Term m))
  | ExpressionRecord [Field m]
  | ExpressionSet (Set (Term m))
  | ExpressionTypeAbstraction (TypeAbstraction m)
  | ExpressionTypeApplication (TypeApplication m)
  | ExpressionUnion (Field m)
  | ExpressionVariable Variable
  deriving (Eq, Ord, Read, Show)

_Expression = "Expression"

_Expression_application = "application"

_Expression_literal = "literal"

_Expression_element = "element"

_Expression_function = "function"

_Expression_let = "let"

_Expression_list = "list"

_Expression_map = "map"

_Expression_nominal = "nominal"

_Expression_optional = "optional"

_Expression_record = "record"

_Expression_set = "set"

_Expression_typeAbstraction = "typeAbstraction"

_Expression_typeApplication = "typeApplication"

_Expression_union = "union"

_Expression_variable = "variable"

-- A labeled term
data Field m 
  = Field {
    fieldName :: FieldName,
    fieldTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Field = "Field"

_Field_name = "name"

_Field_term = "term"

-- The name of a field
type FieldName = String

_FieldName = "FieldName"

-- The name and type of a field
data FieldType 
  = FieldType {
    fieldTypeName :: FieldName,
    fieldTypeType :: Type}
  deriving (Eq, Ord, Read, Show)

_FieldType = "FieldType"

_FieldType_name = "name"

_FieldType_type = "type"

-- A floating-point type
data FloatType 
  = FloatTypeBigfloat 
  | FloatTypeFloat32 
  | FloatTypeFloat64 
  deriving (Eq, Ord, Read, Show)

_FloatType = "FloatType"

_FloatType_bigfloat = "bigfloat"

_FloatType_float32 = "float32"

_FloatType_float64 = "float64"

-- A floating-point literal value
data FloatValue 
  = FloatValueBigfloat Double
  | FloatValueFloat32 Float
  | FloatValueFloat64 Double
  deriving (Eq, Ord, Read, Show)

_FloatValue = "FloatValue"

_FloatValue_bigfloat = "bigfloat"

_FloatValue_float32 = "float32"

_FloatValue_float64 = "float64"

-- A function
data Function m 
  = FunctionCases [Field m]
  | FunctionCompareTo (Term m)
  | FunctionData 
  | FunctionLambda (Lambda m)
  | FunctionOptionalCases (OptionalCases m)
  | FunctionPrimitive Name
  | FunctionProjection FieldName
  deriving (Eq, Ord, Read, Show)

_Function = "Function"

_Function_cases = "cases"

_Function_compareTo = "compareTo"

_Function_data = "data"

_Function_lambda = "lambda"

_Function_optionalCases = "optionalCases"

_Function_primitive = "primitive"

_Function_projection = "projection"

-- A function type, also known as an arrow type
data FunctionType 
  = FunctionType {
    functionTypeDomain :: Type,
    functionTypeCodomain :: Type}
  deriving (Eq, Ord, Read, Show)

_FunctionType = "FunctionType"

_FunctionType_domain = "domain"

_FunctionType_codomain = "codomain"

-- The identifier of a function constructor
data FunctionVariant 
  = FunctionVariantCases 
  | FunctionVariantCompareTo 
  | FunctionVariantData 
  | FunctionVariantLambda 
  | FunctionVariantOptionalCases 
  | FunctionVariantPrimitive 
  | FunctionVariantProjection 
  deriving (Eq, Ord, Read, Show)

_FunctionVariant = "FunctionVariant"

_FunctionVariant_cases = "cases"

_FunctionVariant_compareTo = "compareTo"

_FunctionVariant_data = "data"

_FunctionVariant_lambda = "lambda"

_FunctionVariant_optionalCases = "optionalCases"

_FunctionVariant_primitive = "primitive"

_FunctionVariant_projection = "projection"

-- An integer type
data IntegerType 
  = IntegerTypeBigint 
  | IntegerTypeInt8 
  | IntegerTypeInt16 
  | IntegerTypeInt32 
  | IntegerTypeInt64 
  | IntegerTypeUint8 
  | IntegerTypeUint16 
  | IntegerTypeUint32 
  | IntegerTypeUint64 
  deriving (Eq, Ord, Read, Show)

_IntegerType = "IntegerType"

_IntegerType_bigint = "bigint"

_IntegerType_int8 = "int8"

_IntegerType_int16 = "int16"

_IntegerType_int32 = "int32"

_IntegerType_int64 = "int64"

_IntegerType_uint8 = "uint8"

_IntegerType_uint16 = "uint16"

_IntegerType_uint32 = "uint32"

_IntegerType_uint64 = "uint64"

-- An integer literal value
data IntegerValue 
  = IntegerValueBigint Integer
  | IntegerValueInt8 Int
  | IntegerValueInt16 Int
  | IntegerValueInt32 Int
  | IntegerValueInt64 Integer
  | IntegerValueUint8 Int
  | IntegerValueUint16 Int
  | IntegerValueUint32 Integer
  | IntegerValueUint64 Integer
  deriving (Eq, Ord, Read, Show)

_IntegerValue = "IntegerValue"

_IntegerValue_bigint = "bigint"

_IntegerValue_int8 = "int8"

_IntegerValue_int16 = "int16"

_IntegerValue_int32 = "int32"

_IntegerValue_int64 = "int64"

_IntegerValue_uint8 = "uint8"

_IntegerValue_uint16 = "uint16"

_IntegerValue_uint32 = "uint32"

_IntegerValue_uint64 = "uint64"

-- A function abstraction (lambda)
data Lambda m 
  = Lambda {
    lambdaParameter :: Variable,
    lambdaBody :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Lambda = "Lambda"

_Lambda_parameter = "parameter"

_Lambda_body = "body"

-- A 'let' binding
data Let m 
  = Let {
    letKey :: Variable,
    letValue :: (Term m),
    letEnvironment :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Let = "Let"

_Let_key = "key"

_Let_value = "value"

_Let_environment = "environment"

-- A term constant; an instance of a literal type
data Literal 
  = LiteralBinary String
  | LiteralBoolean BooleanValue
  | LiteralFloat FloatValue
  | LiteralInteger IntegerValue
  | LiteralString String
  deriving (Eq, Ord, Read, Show)

_Literal = "Literal"

_Literal_binary = "binary"

_Literal_boolean = "boolean"

_Literal_float = "float"

_Literal_integer = "integer"

_Literal_string = "string"

-- Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants
data LiteralType 
  = LiteralTypeBinary 
  | LiteralTypeBoolean 
  | LiteralTypeFloat FloatType
  | LiteralTypeInteger IntegerType
  | LiteralTypeString 
  deriving (Eq, Ord, Read, Show)

_LiteralType = "LiteralType"

_LiteralType_binary = "binary"

_LiteralType_boolean = "boolean"

_LiteralType_float = "float"

_LiteralType_integer = "integer"

_LiteralType_string = "string"

-- The identifier of a literal constructor
data LiteralVariant 
  = LiteralVariantBinary 
  | LiteralVariantBoolean 
  | LiteralVariantFloat 
  | LiteralVariantInteger 
  | LiteralVariantString 
  deriving (Eq, Ord, Read, Show)

_LiteralVariant = "LiteralVariant"

_LiteralVariant_binary = "binary"

_LiteralVariant_boolean = "boolean"

_LiteralVariant_float = "float"

_LiteralVariant_integer = "integer"

_LiteralVariant_string = "string"

-- A map type
data MapType 
  = MapType {
    mapTypeKeys :: Type,
    mapTypeValues :: Type}
  deriving (Eq, Ord, Read, Show)

_MapType = "MapType"

_MapType_keys = "keys"

_MapType_values = "values"

-- A built-in metadata container for terms
data Meta 
  = Meta {
    metaDescription :: (Maybe String),
    metaType :: (Maybe Type)}
  deriving (Eq, Ord, Read, Show)

_Meta = "Meta"

_Meta_description = "description"

_Meta_type = "type"

-- A unique element name
type Name = String

_Name = "Name"

-- A term annotated with a fixed, named type; an instance of a newtype
data NominalTerm m 
  = NominalTerm {
    nominalTermTypeName :: Name,
    nominalTermTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_NominalTerm = "NominalTerm"

_NominalTerm_typeName = "typeName"

_NominalTerm_term = "term"

-- A case statement for matching optional terms
data OptionalCases m 
  = OptionalCases {
    optionalCasesNothing :: (Term m),
    optionalCasesJust :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_OptionalCases = "OptionalCases"

_OptionalCases_nothing = "nothing"

_OptionalCases_just = "just"

-- An encoded optional value, for languages which do not natively support optionals
data OptionalExpression m 
  = OptionalExpressionJust (Term m)
  | OptionalExpressionNothing 
  deriving (Eq, Ord, Read, Show)

_OptionalExpression = "OptionalExpression"

_OptionalExpression_just = "just"

_OptionalExpression_nothing = "nothing"

-- Numeric precision: arbitrary precision, or precision to a specified number of bits
data Precision 
  = PrecisionArbitrary 
  | PrecisionBits Int
  deriving (Eq, Ord, Read, Show)

_Precision = "Precision"

_Precision_arbitrary = "arbitrary"

_Precision_bits = "bits"

-- A data term
data Term m 
  = Term {
    termData :: (Expression m),
    termMeta :: m}
  deriving (Eq, Ord, Read, Show)

_Term = "Term"

_Term_data = "data"

_Term_meta = "meta"

-- The identifier of a term expression constructor
data TermVariant 
  = TermVariantApplication 
  | TermVariantElement 
  | TermVariantFunction 
  | TermVariantLet 
  | TermVariantList 
  | TermVariantLiteral 
  | TermVariantMap 
  | TermVariantNominal 
  | TermVariantOptional 
  | TermVariantRecord 
  | TermVariantSet 
  | TermVariantTypeAbstraction 
  | TermVariantTypeApplication 
  | TermVariantUnion 
  | TermVariantVariable 
  deriving (Eq, Ord, Read, Show)

_TermVariant = "TermVariant"

_TermVariant_application = "application"

_TermVariant_element = "element"

_TermVariant_function = "function"

_TermVariant_let = "let"

_TermVariant_list = "list"

_TermVariant_literal = "literal"

_TermVariant_map = "map"

_TermVariant_nominal = "nominal"

_TermVariant_optional = "optional"

_TermVariant_record = "record"

_TermVariant_set = "set"

_TermVariant_typeAbstraction = "typeAbstraction"

_TermVariant_typeApplication = "typeApplication"

_TermVariant_union = "union"

_TermVariant_variable = "variable"

-- A data type
data Type 
  = TypeLiteral LiteralType
  | TypeElement Type
  | TypeFunction FunctionType
  | TypeList Type
  | TypeMap MapType
  | TypeNominal Name
  | TypeOptional Type
  | TypeRecord [FieldType]
  | TypeSet Type
  | TypeUnion [FieldType]
  | TypeUniversal UniversalType
  | TypeVariable TypeVariable
  deriving (Eq, Ord, Read, Show)

_Type = "Type"

_Type_literal = "literal"

_Type_element = "element"

_Type_function = "function"

_Type_list = "list"

_Type_map = "map"

_Type_nominal = "nominal"

_Type_optional = "optional"

_Type_record = "record"

_Type_set = "set"

_Type_union = "union"

_Type_universal = "universal"

_Type_variable = "variable"

-- A type abstraction (generalization), which binds a type variable to a term
data TypeAbstraction m 
  = TypeAbstraction {
    typeAbstractionParameter :: TypeVariable,
    typeAbstractionBody :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_TypeAbstraction = "TypeAbstraction"

_TypeAbstraction_parameter = "parameter"

_TypeAbstraction_body = "body"

-- A type application (instantiation), which applies a term to a type
data TypeApplication m 
  = TypeApplication {
    typeApplicationFunction :: (Term m),
    typeApplicationArgument :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeApplication = "TypeApplication"

_TypeApplication_function = "function"

_TypeApplication_argument = "argument"

-- A type expression together with free type variables occurring in the expression
data TypeScheme 
  = TypeScheme {
    typeSchemeVariables :: [TypeVariable],
    typeSchemeType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeScheme = "TypeScheme"

_TypeScheme_variables = "variables"

_TypeScheme_type = "type"

-- A symbol which stands in for a type
type TypeVariable = String

_TypeVariable = "TypeVariable"

-- The identifier of a type constructor
data TypeVariant 
  = TypeVariantElement 
  | TypeVariantFunction 
  | TypeVariantList 
  | TypeVariantLiteral 
  | TypeVariantMap 
  | TypeVariantNominal 
  | TypeVariantOptional 
  | TypeVariantRecord 
  | TypeVariantSet 
  | TypeVariantUnion 
  | TypeVariantUniversal 
  | TypeVariantVariable 
  deriving (Eq, Ord, Read, Show)

_TypeVariant = "TypeVariant"

_TypeVariant_element = "element"

_TypeVariant_function = "function"

_TypeVariant_list = "list"

_TypeVariant_literal = "literal"

_TypeVariant_map = "map"

_TypeVariant_nominal = "nominal"

_TypeVariant_optional = "optional"

_TypeVariant_record = "record"

_TypeVariant_set = "set"

_TypeVariant_union = "union"

_TypeVariant_universal = "universal"

_TypeVariant_variable = "variable"

-- A type together with an instance of the type
data TypedTerm m 
  = TypedTerm {
    typedTermType :: Type,
    typedTermTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_TypedTerm = "TypedTerm"

_TypedTerm_type = "type"

_TypedTerm_term = "term"

-- A universally quantified ('forall') type, parameterized by a type variable
data UniversalType 
  = UniversalType {
    universalTypeVariable :: String,
    universalTypeBody :: Type}
  deriving (Eq, Ord, Read, Show)

_UniversalType = "UniversalType"

_UniversalType_variable = "variable"

_UniversalType_body = "body"

-- A symbol which stands in for a term
type Variable = String

_Variable = "Variable"