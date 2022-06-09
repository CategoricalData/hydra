module Hydra.Core where

import Data.Map
import Data.Set

-- A term which applies a function to an argument
data Application m 
  = Application {
    applicationFunction :: (Data m),
    applicationArgument :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_Application = (Name "hydra/core.Application")

_Application_function = (FieldName "function")

_Application_argument = (FieldName "argument")

-- A boolean literal value
data BooleanValue 
  = BooleanValueFalse 
  | BooleanValueTrue 
  deriving (Eq, Ord, Read, Show)

_BooleanValue = (Name "hydra/core.BooleanValue")

_BooleanValue_false = (FieldName "false")

_BooleanValue_true = (FieldName "true")

-- An equality judgement: less than, equal to, or greater than
data Comparison 
  = ComparisonLessThan 
  | ComparisonEqualTo 
  | ComparisonGreaterThan 
  deriving (Eq, Ord, Read, Show)

_Comparison = (Name "hydra/core.Comparison")

_Comparison_lessThan = (FieldName "lessThan")

_Comparison_equalTo = (FieldName "equalTo")

_Comparison_greaterThan = (FieldName "greaterThan")

-- A data term
data Data m 
  = Data {
    dataTerm :: (DataTerm m),
    dataMeta :: m}
  deriving (Eq, Ord, Read, Show)

_Data = (Name "hydra/core.Data")

_Data_term = (FieldName "term")

_Data_meta = (FieldName "meta")

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

_DataTerm = (Name "hydra/core.DataTerm")

_DataTerm_application = (FieldName "application")

_DataTerm_literal = (FieldName "literal")

_DataTerm_element = (FieldName "element")

_DataTerm_function = (FieldName "function")

_DataTerm_let = (FieldName "let")

_DataTerm_list = (FieldName "list")

_DataTerm_map = (FieldName "map")

_DataTerm_nominal = (FieldName "nominal")

_DataTerm_optional = (FieldName "optional")

_DataTerm_record = (FieldName "record")

_DataTerm_set = (FieldName "set")

_DataTerm_typeAbstraction = (FieldName "typeAbstraction")

_DataTerm_typeApplication = (FieldName "typeApplication")

_DataTerm_union = (FieldName "union")

_DataTerm_variable = (FieldName "variable")

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

_DataVariant = (Name "hydra/core.DataVariant")

_DataVariant_application = (FieldName "application")

_DataVariant_element = (FieldName "element")

_DataVariant_function = (FieldName "function")

_DataVariant_let = (FieldName "let")

_DataVariant_list = (FieldName "list")

_DataVariant_literal = (FieldName "literal")

_DataVariant_map = (FieldName "map")

_DataVariant_nominal = (FieldName "nominal")

_DataVariant_optional = (FieldName "optional")

_DataVariant_record = (FieldName "record")

_DataVariant_set = (FieldName "set")

_DataVariant_typeAbstraction = (FieldName "typeAbstraction")

_DataVariant_typeApplication = (FieldName "typeApplication")

_DataVariant_union = (FieldName "union")

_DataVariant_variable = (FieldName "variable")

-- A corresponding elimination for an introduction term
data Elimination m 
  = EliminationElement 
  | EliminationNominal Name
  | EliminationOptional (OptionalCases m)
  | EliminationRecord FieldName
  | EliminationUnion [Field m]
  deriving (Eq, Ord, Read, Show)

_Elimination = (Name "hydra/core.Elimination")

_Elimination_element = (FieldName "element")

_Elimination_nominal = (FieldName "nominal")

_Elimination_optional = (FieldName "optional")

_Elimination_record = (FieldName "record")

_Elimination_union = (FieldName "union")

-- The identifier of an elimination constructor
data EliminationVariant 
  = EliminationVariantElement 
  | EliminationVariantNominal 
  | EliminationVariantOptional 
  | EliminationVariantRecord 
  | EliminationVariantUnion 
  deriving (Eq, Ord, Read, Show)

_EliminationVariant = (Name "hydra/core.EliminationVariant")

_EliminationVariant_element = (FieldName "element")

_EliminationVariant_nominal = (FieldName "nominal")

_EliminationVariant_optional = (FieldName "optional")

_EliminationVariant_record = (FieldName "record")

_EliminationVariant_union = (FieldName "union")

-- A labeled term
data Field m 
  = Field {
    fieldName :: FieldName,
    fieldData :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_Field = (Name "hydra/core.Field")

_Field_name = (FieldName "name")

_Field_data = (FieldName "data")

-- The name of a field
newtype FieldName 
  = FieldName {
    unFieldName :: String}
  deriving (Eq, Ord, Read, Show)

_FieldName = (Name "hydra/core.FieldName")

-- The name and type of a field
data FieldType m 
  = FieldType {
    fieldTypeName :: FieldName,
    fieldTypeType :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_FieldType = (Name "hydra/core.FieldType")

_FieldType_name = (FieldName "name")

_FieldType_type = (FieldName "type")

-- A floating-point type
data FloatType 
  = FloatTypeBigfloat 
  | FloatTypeFloat32 
  | FloatTypeFloat64 
  deriving (Eq, Ord, Read, Show)

_FloatType = (Name "hydra/core.FloatType")

_FloatType_bigfloat = (FieldName "bigfloat")

_FloatType_float32 = (FieldName "float32")

_FloatType_float64 = (FieldName "float64")

-- A floating-point literal value
data FloatValue 
  = FloatValueBigfloat Double
  | FloatValueFloat32 Float
  | FloatValueFloat64 Double
  deriving (Eq, Ord, Read, Show)

_FloatValue = (Name "hydra/core.FloatValue")

_FloatValue_bigfloat = (FieldName "bigfloat")

_FloatValue_float32 = (FieldName "float32")

_FloatValue_float64 = (FieldName "float64")

-- A function
data Function m 
  = FunctionCompareTo (Data m)
  | FunctionElimination (Elimination m)
  | FunctionLambda (Lambda m)
  | FunctionPrimitive Name
  deriving (Eq, Ord, Read, Show)

_Function = (Name "hydra/core.Function")

_Function_compareTo = (FieldName "compareTo")

_Function_elimination = (FieldName "elimination")

_Function_lambda = (FieldName "lambda")

_Function_primitive = (FieldName "primitive")

-- A function type, also known as an arrow type
data FunctionType m 
  = FunctionType {
    functionTypeDomain :: (Type m),
    functionTypeCodomain :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_FunctionType = (Name "hydra/core.FunctionType")

_FunctionType_domain = (FieldName "domain")

_FunctionType_codomain = (FieldName "codomain")

-- The identifier of a function constructor
data FunctionVariant 
  = FunctionVariantCompareTo 
  | FunctionVariantElimination 
  | FunctionVariantLambda 
  | FunctionVariantPrimitive 
  deriving (Eq, Ord, Read, Show)

_FunctionVariant = (Name "hydra/core.FunctionVariant")

_FunctionVariant_compareTo = (FieldName "compareTo")

_FunctionVariant_elimination = (FieldName "elimination")

_FunctionVariant_lambda = (FieldName "lambda")

_FunctionVariant_primitive = (FieldName "primitive")

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

_IntegerType = (Name "hydra/core.IntegerType")

_IntegerType_bigint = (FieldName "bigint")

_IntegerType_int8 = (FieldName "int8")

_IntegerType_int16 = (FieldName "int16")

_IntegerType_int32 = (FieldName "int32")

_IntegerType_int64 = (FieldName "int64")

_IntegerType_uint8 = (FieldName "uint8")

_IntegerType_uint16 = (FieldName "uint16")

_IntegerType_uint32 = (FieldName "uint32")

_IntegerType_uint64 = (FieldName "uint64")

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

_IntegerValue = (Name "hydra/core.IntegerValue")

_IntegerValue_bigint = (FieldName "bigint")

_IntegerValue_int8 = (FieldName "int8")

_IntegerValue_int16 = (FieldName "int16")

_IntegerValue_int32 = (FieldName "int32")

_IntegerValue_int64 = (FieldName "int64")

_IntegerValue_uint8 = (FieldName "uint8")

_IntegerValue_uint16 = (FieldName "uint16")

_IntegerValue_uint32 = (FieldName "uint32")

_IntegerValue_uint64 = (FieldName "uint64")

-- A function abstraction (lambda)
data Lambda m 
  = Lambda {
    lambdaParameter :: Variable,
    lambdaBody :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_Lambda = (Name "hydra/core.Lambda")

_Lambda_parameter = (FieldName "parameter")

_Lambda_body = (FieldName "body")

-- A 'let' binding
data Let m 
  = Let {
    letKey :: Variable,
    letValue :: (Data m),
    letEnvironment :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_Let = (Name "hydra/core.Let")

_Let_key = (FieldName "key")

_Let_value = (FieldName "value")

_Let_environment = (FieldName "environment")

-- A term constant; an instance of a literal type
data Literal 
  = LiteralBinary String
  | LiteralBoolean BooleanValue
  | LiteralFloat FloatValue
  | LiteralInteger IntegerValue
  | LiteralString String
  deriving (Eq, Ord, Read, Show)

_Literal = (Name "hydra/core.Literal")

_Literal_binary = (FieldName "binary")

_Literal_boolean = (FieldName "boolean")

_Literal_float = (FieldName "float")

_Literal_integer = (FieldName "integer")

_Literal_string = (FieldName "string")

-- Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants
data LiteralType 
  = LiteralTypeBinary 
  | LiteralTypeBoolean 
  | LiteralTypeFloat FloatType
  | LiteralTypeInteger IntegerType
  | LiteralTypeString 
  deriving (Eq, Ord, Read, Show)

_LiteralType = (Name "hydra/core.LiteralType")

_LiteralType_binary = (FieldName "binary")

_LiteralType_boolean = (FieldName "boolean")

_LiteralType_float = (FieldName "float")

_LiteralType_integer = (FieldName "integer")

_LiteralType_string = (FieldName "string")

-- The identifier of a literal constructor
data LiteralVariant 
  = LiteralVariantBinary 
  | LiteralVariantBoolean 
  | LiteralVariantFloat 
  | LiteralVariantInteger 
  | LiteralVariantString 
  deriving (Eq, Ord, Read, Show)

_LiteralVariant = (Name "hydra/core.LiteralVariant")

_LiteralVariant_binary = (FieldName "binary")

_LiteralVariant_boolean = (FieldName "boolean")

_LiteralVariant_float = (FieldName "float")

_LiteralVariant_integer = (FieldName "integer")

_LiteralVariant_string = (FieldName "string")

-- A map type
data MapType m 
  = MapType {
    mapTypeKeys :: (Type m),
    mapTypeValues :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_MapType = (Name "hydra/core.MapType")

_MapType_keys = (FieldName "keys")

_MapType_values = (FieldName "values")

-- A built-in metadata container for terms
data Meta 
  = Meta {
    metaAnnotations :: (Map String (Data Meta))}
  deriving (Eq, Ord, Read, Show)

_Meta = (Name "hydra/core.Meta")

_Meta_annotations = (FieldName "annotations")

-- A unique element name
newtype Name 
  = Name {
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Name "hydra/core.Name")

-- A term annotated with a fixed, named type; an instance of a newtype
data Named m 
  = Named {
    namedTypeName :: Name,
    namedTerm :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_Named = (Name "hydra/core.Named")

_Named_typeName = (FieldName "typeName")

_Named_term = (FieldName "term")

-- A case statement for matching optional terms
data OptionalCases m 
  = OptionalCases {
    optionalCasesNothing :: (Data m),
    optionalCasesJust :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_OptionalCases = (Name "hydra/core.OptionalCases")

_OptionalCases_nothing = (FieldName "nothing")

_OptionalCases_just = (FieldName "just")

-- Numeric precision: arbitrary precision, or precision to a specified number of bits
data Precision 
  = PrecisionArbitrary 
  | PrecisionBits Int
  deriving (Eq, Ord, Read, Show)

_Precision = (Name "hydra/core.Precision")

_Precision_arbitrary = (FieldName "arbitrary")

_Precision_bits = (FieldName "bits")

-- A data type
data Type m 
  = Type {
    typeTerm :: (TypeTerm m),
    typeMeta :: m}
  deriving (Eq, Ord, Read, Show)

_Type = (Name "hydra/core.Type")

_Type_term = (FieldName "term")

_Type_meta = (FieldName "meta")

-- A type abstraction (generalization), which binds a type variable to a term
data TypeAbstraction m 
  = TypeAbstraction {
    typeAbstractionParameter :: TypeVariable,
    typeAbstractionBody :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_TypeAbstraction = (Name "hydra/core.TypeAbstraction")

_TypeAbstraction_parameter = (FieldName "parameter")

_TypeAbstraction_body = (FieldName "body")

-- A type application (instantiation), which applies a term to a type
data TypeApplication m 
  = TypeApplication {
    typeApplicationFunction :: (Data m),
    typeApplicationArgument :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_TypeApplication = (Name "hydra/core.TypeApplication")

_TypeApplication_function = (FieldName "function")

_TypeApplication_argument = (FieldName "argument")

-- A type expression together with free type variables occurring in the expression
data TypeScheme m 
  = TypeScheme {
    typeSchemeVariables :: [TypeVariable],
    typeSchemeType :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_TypeScheme = (Name "hydra/core.TypeScheme")

_TypeScheme_variables = (FieldName "variables")

_TypeScheme_type = (FieldName "type")

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

_TypeTerm = (Name "hydra/core.TypeTerm")

_TypeTerm_element = (FieldName "element")

_TypeTerm_function = (FieldName "function")

_TypeTerm_list = (FieldName "list")

_TypeTerm_literal = (FieldName "literal")

_TypeTerm_map = (FieldName "map")

_TypeTerm_nominal = (FieldName "nominal")

_TypeTerm_optional = (FieldName "optional")

_TypeTerm_record = (FieldName "record")

_TypeTerm_set = (FieldName "set")

_TypeTerm_union = (FieldName "union")

_TypeTerm_universal = (FieldName "universal")

_TypeTerm_variable = (FieldName "variable")

-- A symbol which stands in for a type
newtype TypeVariable 
  = TypeVariable {
    unTypeVariable :: String}
  deriving (Eq, Ord, Read, Show)

_TypeVariable = (Name "hydra/core.TypeVariable")

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

_TypeVariant = (Name "hydra/core.TypeVariant")

_TypeVariant_element = (FieldName "element")

_TypeVariant_function = (FieldName "function")

_TypeVariant_list = (FieldName "list")

_TypeVariant_literal = (FieldName "literal")

_TypeVariant_map = (FieldName "map")

_TypeVariant_nominal = (FieldName "nominal")

_TypeVariant_optional = (FieldName "optional")

_TypeVariant_record = (FieldName "record")

_TypeVariant_set = (FieldName "set")

_TypeVariant_union = (FieldName "union")

_TypeVariant_universal = (FieldName "universal")

_TypeVariant_variable = (FieldName "variable")

-- A type together with an instance of the type
data TypedData m 
  = TypedData {
    typedDataType :: (Type m),
    typedDataTerm :: (Data m)}
  deriving (Eq, Ord, Read, Show)

_TypedData = (Name "hydra/core.TypedData")

_TypedData_type = (FieldName "type")

_TypedData_term = (FieldName "term")

-- A universally quantified ('forall') type, parameterized by a type variable
data UniversalType m 
  = UniversalType {
    universalTypeVariable :: TypeVariable,
    universalTypeBody :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_UniversalType = (Name "hydra/core.UniversalType")

_UniversalType_variable = (FieldName "variable")

_UniversalType_body = (FieldName "body")

-- A symbol which stands in for a term
newtype Variable 
  = Variable {
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = (Name "hydra/core.Variable")