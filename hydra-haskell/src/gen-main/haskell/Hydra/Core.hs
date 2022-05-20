module Hydra.Core where

import Data.Map
import Data.Set

-- A term which applies a function to an argument
data Application m 
  = Application {
    applicationFunction :: (Data m),
    applicationArgument :: (Data m)}
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

-- A data term
data Data m 
  = Data {
    dataTerm :: (DataTerm m),
    dataMeta :: m}
  deriving (Eq, Ord, Read, Show)

_Data = "hydra/core.Data"

_Data_term = "term"

_Data_meta = "meta"

-- A term expression
data DataTerm m 
  = DataTermApplication (Application m)
  | DataTermLiteral Literal
  | DataTermElement Name
  | DataTermFunction (Function m)
  | DataTermLet (Let m)
  | DataTermList [Data m]
  | DataTermMap (Map (Data m) (Data m))
  | DataTermNominal (Named m)
  | DataTermOptional (Maybe (Data m))
  | DataTermRecord [Field m]
  | DataTermSet (Set (Data m))
  | DataTermTypeAbstraction (TypeAbstraction m)
  | DataTermTypeApplication (TypeApplication m)
  | DataTermUnion (Field m)
  | DataTermVariable Variable
  deriving (Eq, Ord, Read, Show)

_DataTerm = "hydra/core.DataTerm"

_DataTerm_application = "application"

_DataTerm_literal = "literal"

_DataTerm_element = "element"

_DataTerm_function = "function"

_DataTerm_let = "let"

_DataTerm_list = "list"

_DataTerm_map = "map"

_DataTerm_nominal = "nominal"

_DataTerm_optional = "optional"

_DataTerm_record = "record"

_DataTerm_set = "set"

_DataTerm_typeAbstraction = "typeAbstraction"

_DataTerm_typeApplication = "typeApplication"

_DataTerm_union = "union"

_DataTerm_variable = "variable"

-- The identifier of a term expression constructor
data DataVariant 
  = DataVariantApplication 
  | DataVariantElement 
  | DataVariantFunction 
  | DataVariantLet 
  | DataVariantList 
  | DataVariantLiteral 
  | DataVariantMap 
  | DataVariantNominal 
  | DataVariantOptional 
  | DataVariantRecord 
  | DataVariantSet 
  | DataVariantTypeAbstraction 
  | DataVariantTypeApplication 
  | DataVariantUnion 
  | DataVariantVariable 
  deriving (Eq, Ord, Read, Show)

_DataVariant = "hydra/core.DataVariant"

_DataVariant_application = "application"

_DataVariant_element = "element"

_DataVariant_function = "function"

_DataVariant_let = "let"

_DataVariant_list = "list"

_DataVariant_literal = "literal"

_DataVariant_map = "map"

_DataVariant_nominal = "nominal"

_DataVariant_optional = "optional"

_DataVariant_record = "record"

_DataVariant_set = "set"

_DataVariant_typeAbstraction = "typeAbstraction"

_DataVariant_typeApplication = "typeApplication"

_DataVariant_union = "union"

_DataVariant_variable = "variable"

-- A labeled term
data Field m 
  = Field {
    fieldName :: FieldName,
    fieldData :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_Field = "hydra/core.Field"

_Field_name = "name"

_Field_data = "data"

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
  | FunctionCompareTo (Data m)
  | FunctionDelta 
  | FunctionLambda (Lambda m)
  | FunctionOptionalCases (OptionalCases m)
  | FunctionPrimitive Name
  | FunctionProjection FieldName
  deriving (Eq, Ord, Read, Show)

_Function = "hydra/core.Function"

_Function_cases = "cases"

_Function_compareTo = "compareTo"

_Function_delta = "delta"

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
  | FunctionVariantDelta 
  | FunctionVariantLambda 
  | FunctionVariantOptionalCases 
  | FunctionVariantPrimitive 
  | FunctionVariantProjection 
  deriving (Eq, Ord, Read, Show)

_FunctionVariant = "hydra/core.FunctionVariant"

_FunctionVariant_cases = "cases"

_FunctionVariant_compareTo = "compareTo"

_FunctionVariant_delta = "delta"

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
    lambdaBody :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_Lambda = "hydra/core.Lambda"

_Lambda_parameter = "parameter"

_Lambda_body = "body"

-- A 'let' binding
data Let m 
  = Let {
    letKey :: Variable,
    letValue :: (Data m),
    letEnvironment :: (Data m)}
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
  = Meta {metaAnnotations :: (Map Name (Data Meta))}
  deriving (Eq, Ord, Read, Show)

_Meta = "hydra/core.Meta"

_Meta_annotations = "annotations"

-- A unique element name
type Name = String

_Name = "hydra/core.Name"

-- A term annotated with a fixed, named type; an instance of a newtype
data Named m 
  = Named {
    namedTypeName :: Name,
    namedTerm :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_Named = "hydra/core.Named"

_Named_typeName = "typeName"

_Named_term = "term"

-- A case statement for matching optional terms
data OptionalCases m 
  = OptionalCases {
    optionalCasesNothing :: (Data m),
    optionalCasesJust :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_OptionalCases = "hydra/core.OptionalCases"

_OptionalCases_nothing = "nothing"

_OptionalCases_just = "just"

-- Numeric precision: arbitrary precision, or precision to a specified number of bits
data Precision 
  = PrecisionArbitrary 
  | PrecisionBits Int
  deriving (Eq, Ord, Read, Show)

_Precision = "hydra/core.Precision"

_Precision_arbitrary = "arbitrary"

_Precision_bits = "bits"

-- A data type
data Type m 
  = Type {
    typeTerm :: (TypeTerm m),
    typeMeta :: m}
  deriving (Eq, Ord, Read, Show)

_Type = "hydra/core.Type"

_Type_term = "term"

_Type_meta = "meta"

-- A type abstraction (generalization), which binds a type variable to a term
data TypeAbstraction m 
  = TypeAbstraction {
    typeAbstractionParameter :: TypeVariable,
    typeAbstractionBody :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_TypeAbstraction = "hydra/core.TypeAbstraction"

_TypeAbstraction_parameter = "parameter"

_TypeAbstraction_body = "body"

-- A type application (instantiation), which applies a term to a type
data TypeApplication m 
  = TypeApplication {
    typeApplicationFunction :: (Data m),
    typeApplicationArgument :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_TypeApplication = "hydra/core.TypeApplication"

_TypeApplication_function = "function"

_TypeApplication_argument = "argument"

-- A type expression together with free type variables occurring in the expression
data TypeScheme m 
  = TypeScheme {
    typeSchemeVariables :: [TypeVariable],
    typeSchemeType :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_TypeScheme = "hydra/core.TypeScheme"

_TypeScheme_variables = "variables"

_TypeScheme_type = "type"

-- A data type
data TypeTerm m 
  = TypeTermElement (Type m)
  | TypeTermFunction (FunctionType m)
  | TypeTermList (Type m)
  | TypeTermLiteral LiteralType
  | TypeTermMap (MapType m)
  | TypeTermNominal Name
  | TypeTermOptional (Type m)
  | TypeTermRecord [FieldType m]
  | TypeTermSet (Type m)
  | TypeTermUnion [FieldType m]
  | TypeTermUniversal (UniversalType m)
  | TypeTermVariable TypeVariable
  deriving (Eq, Ord, Read, Show)

_TypeTerm = "hydra/core.TypeTerm"

_TypeTerm_element = "element"

_TypeTerm_function = "function"

_TypeTerm_list = "list"

_TypeTerm_literal = "literal"

_TypeTerm_map = "map"

_TypeTerm_nominal = "nominal"

_TypeTerm_optional = "optional"

_TypeTerm_record = "record"

_TypeTerm_set = "set"

_TypeTerm_union = "union"

_TypeTerm_universal = "universal"

_TypeTerm_variable = "variable"

-- A symbol which stands in for a type
newtype TypeVariable 
  = TypeVariable String
  deriving (Eq, Ord, Read, Show)

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
data TypedData m 
  = TypedData {
    typedDataType :: (Type m),
    typedDataTerm :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_TypedData = "hydra/core.TypedData"

_TypedData_type = "type"

_TypedData_term = "term"

-- A universally quantified ('forall') type, parameterized by a type variable
data UniversalType m 
  = UniversalType {
    universalTypeVariable :: TypeVariable,
    universalTypeBody :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_UniversalType = "hydra/core.UniversalType"

_UniversalType_variable = "variable"

_UniversalType_body = "body"

-- A symbol which stands in for a term
type Variable = String

_Variable = "hydra/core.Variable"