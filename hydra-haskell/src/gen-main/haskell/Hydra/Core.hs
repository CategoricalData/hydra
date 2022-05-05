module Hydra.Core where

import Data.Map
import Data.Set

-- A term which applies a function to an argument
data Application m
  = Application {
    applicationFunction :: (Term m),
    applicationArgument :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Application = "hydra/core.Application"

_Application_function = "function"

_Application_argument = "argument"

-- A boolean literal value
data BooleanValue
  = BooleanValueFalse
  | BooleanValueTrue
  deriving (Eq, Ord, Read, Show)

_BooleanValue = "hydra/core.BooleanValue"

_BooleanValue_false = "false"

_BooleanValue_true = "true"

-- An equality judgement: less than, equal to, or greater than
data Comparison
  = ComparisonLessThan
  | ComparisonEqualTo
  | ComparisonGreaterThan
  deriving (Eq, Ord, Read, Show)

_Comparison = "hydra/core.Comparison"

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

_Expression = "hydra/core.Expression"

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

_Field = "hydra/core.Field"

_Field_name = "name"

_Field_term = "term"

-- The name of a field
type FieldName = String

_FieldName = "hydra/core.FieldName"

-- The name and type of a field
data FieldType m
  = FieldType {
    fieldTypeName :: FieldName,
    fieldTypeType :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_FieldType = "hydra/core.FieldType"

_FieldType_name = "name"

_FieldType_type = "type"

-- A floating-point type
data FloatType
  = FloatTypeBigfloat
  | FloatTypeFloat32
  | FloatTypeFloat64
  deriving (Eq, Ord, Read, Show)

_FloatType = "hydra/core.FloatType"

_FloatType_bigfloat = "bigfloat"

_FloatType_float32 = "float32"

_FloatType_float64 = "float64"

-- A floating-point literal value
data FloatValue
  = FloatValueBigfloat Double
  | FloatValueFloat32 Float
  | FloatValueFloat64 Double
  deriving (Eq, Ord, Read, Show)

_FloatValue = "hydra/core.FloatValue"

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

_Function = "hydra/core.Function"

_Function_cases = "cases"

_Function_compareTo = "compareTo"

_Function_data = "data"

_Function_lambda = "lambda"

_Function_optionalCases = "optionalCases"

_Function_primitive = "primitive"

_Function_projection = "projection"

-- A function type, also known as an arrow type
data FunctionType m
  = FunctionType {
    functionTypeDomain :: (Type m),
    functionTypeCodomain :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_FunctionType = "hydra/core.FunctionType"

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

_FunctionVariant = "hydra/core.FunctionVariant"

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

_IntegerType = "hydra/core.IntegerType"

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

_IntegerValue = "hydra/core.IntegerValue"

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

_Lambda = "hydra/core.Lambda"

_Lambda_parameter = "parameter"

_Lambda_body = "body"

-- A 'let' binding
data Let m
  = Let {
    letKey :: Variable,
    letValue :: (Term m),
    letEnvironment :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Let = "hydra/core.Let"

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

_Literal = "hydra/core.Literal"

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

_LiteralType = "hydra/core.LiteralType"

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

_LiteralVariant = "hydra/core.LiteralVariant"

_LiteralVariant_binary = "binary"

_LiteralVariant_boolean = "boolean"

_LiteralVariant_float = "float"

_LiteralVariant_integer = "integer"

_LiteralVariant_string = "string"

-- A map type
data MapType m
  = MapType {
    mapTypeKeys :: (Type m),
    mapTypeValues :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_MapType = "hydra/core.MapType"

_MapType_keys = "keys"

_MapType_values = "values"

-- A built-in metadata container for terms
data Meta
  = Meta {
    metaDescription :: (Maybe String),
    metaType :: (Maybe (Type Meta))}
  deriving (Eq, Ord, Read, Show)

_Meta = "hydra/core.Meta"

_Meta_description = "description"

_Meta_type = "type"

-- A unique element name
type Name = String

_Name = "hydra/core.Name"

-- A term annotated with a fixed, named type; an instance of a newtype
data NominalTerm m
  = NominalTerm {
    nominalTermTypeName :: Name,
    nominalTermTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_NominalTerm = "hydra/core.NominalTerm"

_NominalTerm_typeName = "typeName"

_NominalTerm_term = "term"

-- A case statement for matching optional terms
data OptionalCases m
  = OptionalCases {
    optionalCasesNothing :: (Term m),
    optionalCasesJust :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_OptionalCases = "hydra/core.OptionalCases"

_OptionalCases_nothing = "nothing"

_OptionalCases_just = "just"

-- An encoded optional value, for languages which do not natively support optionals
data OptionalExpression m
  = OptionalExpressionJust (Term m)
  | OptionalExpressionNothing
  deriving (Eq, Ord, Read, Show)

_OptionalExpression = "hydra/core.OptionalExpression"

_OptionalExpression_just = "just"

_OptionalExpression_nothing = "nothing"

-- Numeric precision: arbitrary precision, or precision to a specified number of bits
data Precision
  = PrecisionArbitrary
  | PrecisionBits Int
  deriving (Eq, Ord, Read, Show)

_Precision = "hydra/core.Precision"

_Precision_arbitrary = "arbitrary"

_Precision_bits = "bits"

-- A data term
data Term m
  = Term {
    termData :: (Expression m),
    termMeta :: m}
  deriving (Eq, Ord, Read, Show)

_Term = "hydra/core.Term"

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

_TermVariant = "hydra/core.TermVariant"

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
data Type m
  = Type {
    typeData :: (TypeExpr m),
    typeMeta :: m}
  deriving (Eq, Ord, Read, Show)

_Type = "hydra/core.Type"

_Type_data = "data"

_Type_meta = "meta"

-- A data type
data TypeExpr m
  = TypeExprElement (Type m)
  | TypeExprFunction (FunctionType m)
  | TypeExprList (Type m)
  | TypeExprLiteral LiteralType
  | TypeExprMap (MapType m)
  | TypeExprNominal Name
  | TypeExprOptional (Type m)
  | TypeExprRecord [FieldType m]
  | TypeExprSet (Type m)
  | TypeExprUnion [FieldType m]
  | TypeExprUniversal (UniversalType m)
  | TypeExprVariable TypeVariable
  deriving (Eq, Ord, Read, Show)

_TypeExpr = "hydra/core.TypeExpr"

_TypeExpr_element = "element"

_TypeExpr_function = "function"

_TypeExpr_list = "list"

_TypeExpr_literal = "literal"

_TypeExpr_map = "map"

_TypeExpr_nominal = "nominal"

_TypeExpr_optional = "optional"

_TypeExpr_record = "record"

_TypeExpr_set = "set"

_TypeExpr_union = "union"

_TypeExpr_universal = "universal"

_TypeExpr_variable = "variable"

-- A type abstraction (generalization), which binds a type variable to a term
data TypeAbstraction m
  = TypeAbstraction {
    typeAbstractionParameter :: TypeVariable,
    typeAbstractionBody :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_TypeAbstraction = "hydra/core.TypeAbstraction"

_TypeAbstraction_parameter = "parameter"

_TypeAbstraction_body = "body"

-- A type application (instantiation), which applies a term to a type
data TypeApplication m
  = TypeApplication {
    typeApplicationFunction :: (Term m),
    typeApplicationArgument :: Type m}
  deriving (Eq, Ord, Read, Show)

_TypeApplication = "hydra/core.TypeApplication"

_TypeApplication_function = "function"

_TypeApplication_argument = "argument"

-- A type expression together with free type variables occurring in the expression
data TypeScheme m
  = TypeScheme {
    typeSchemeVariables :: [TypeVariable],
    typeSchemeType :: Type m}
  deriving (Eq, Ord, Read, Show)

_TypeScheme = "hydra/core.TypeScheme"

_TypeScheme_variables = "variables"

_TypeScheme_type = "type"

-- A symbol which stands in for a type
type TypeVariable = String

_TypeVariable = "hydra/core.TypeVariable"

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

_TypeVariant = "hydra/core.TypeVariant"

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
    typedTermType :: Type m,
    typedTermTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_TypedTerm = "hydra/core.TypedTerm"

_TypedTerm_type = "type"

_TypedTerm_term = "term"

-- A universally quantified ('forall') type, parameterized by a type variable
data UniversalType m
  = UniversalType {
    universalTypeVariable :: String,
    universalTypeBody :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_UniversalType = "hydra/core.UniversalType"

_UniversalType_variable = "variable"

_UniversalType_body = "body"

-- A symbol which stands in for a term
type Variable = String

_Variable = "hydra/core.Variable"