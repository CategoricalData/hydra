module Hydra.Core where

import Data.Map
import Data.Set

data Annotated a m 
  = Annotated {
    annotatedSubject :: a,
    annotatedAnnotation :: m}
  deriving (Eq, Ord, Read, Show)

_Annotated = (Name "hydra/core.Annotated")

_Annotated_subject = (FieldName "subject")

_Annotated_annotation = (FieldName "annotation")

-- A term which applies a function to an argument
data Application m 
  = Application {
    -- The left-hand side of the application
    applicationFunction :: (Term m),
    -- The right-hand side of the application
    applicationArgument :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Application = (Name "hydra/core.Application")

_Application_function = (FieldName "function")

_Application_argument = (FieldName "argument")

-- The type-level analog of an application term
data ApplicationType m 
  = ApplicationType {
    -- The left-hand side of the application
    applicationTypeFunction :: (Type m),
    -- The right-hand side of the application
    applicationTypeArgument :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_ApplicationType = (Name "hydra/core.ApplicationType")

_ApplicationType_function = (FieldName "function")

_ApplicationType_argument = (FieldName "argument")

data CaseStatement m 
  = CaseStatement {
    caseStatementTypeName :: Name,
    caseStatementCases :: [Field m]}
  deriving (Eq, Ord, Read, Show)

_CaseStatement = (Name "hydra/core.CaseStatement")

_CaseStatement_typeName = (FieldName "typeName")

_CaseStatement_cases = (FieldName "cases")

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

-- A corresponding elimination for an introduction term
data Elimination m 
  = EliminationElement 
  | EliminationNominal Name
  | EliminationOptional (OptionalCases m)
  | EliminationRecord Projection
  | EliminationUnion (CaseStatement m)
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
    fieldTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Field = (Name "hydra/core.Field")

_Field_name = (FieldName "name")

_Field_term = (FieldName "term")

-- The name of a field
newtype FieldName 
  = FieldName {
    -- The name of a field
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
  = FunctionCompareTo (Term m)
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
    -- The parameter of the lambda
    lambdaParameter :: Variable,
    -- The body of the lambda
    lambdaBody :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Lambda = (Name "hydra/core.Lambda")

_Lambda_parameter = (FieldName "parameter")

_Lambda_body = (FieldName "body")

-- A type abstraction; the type-level analog of a lambda term
data LambdaType m 
  = LambdaType {
    -- The parameter of the lambda
    lambdaTypeParameter :: VariableType,
    -- The body of the lambda
    lambdaTypeBody :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_LambdaType = (Name "hydra/core.LambdaType")

_LambdaType_parameter = (FieldName "parameter")

_LambdaType_body = (FieldName "body")

-- A 'let' binding
data Let m 
  = Let {
    letKey :: Variable,
    letValue :: (Term m),
    letEnvironment :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Let = (Name "hydra/core.Let")

_Let_key = (FieldName "key")

_Let_value = (FieldName "value")

_Let_environment = (FieldName "environment")

-- A term constant; an instance of a literal type
data Literal 
  = LiteralBinary String
  | LiteralBoolean Bool
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
    -- A map of annotation names to annotation values
    metaAnnotations :: (Map String (Term Meta))}
  deriving (Eq, Ord, Read, Show)

_Meta = (Name "hydra/core.Meta")

_Meta_annotations = (FieldName "annotations")

-- A unique element name
newtype Name 
  = Name {
    -- A unique element name
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Name "hydra/core.Name")

-- A term annotated with a fixed, named type; an instance of a newtype
data Named m 
  = Named {
    namedTypeName :: Name,
    namedTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Named = (Name "hydra/core.Named")

_Named_typeName = (FieldName "typeName")

_Named_term = (FieldName "term")

-- A case statement for matching optional terms
data OptionalCases m 
  = OptionalCases {
    -- A term provided if the optional value is nothing
    optionalCasesNothing :: (Term m),
    -- A function which is applied of the optional value is non-nothing
    optionalCasesJust :: (Term m)}
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

data Projection 
  = Projection {
    projectionTypeName :: Name,
    projectionField :: FieldName}
  deriving (Eq, Ord, Read, Show)

_Projection = (Name "hydra/core.Projection")

_Projection_typeName = (FieldName "typeName")

_Projection_field = (FieldName "field")

-- A record, or labeled tuple; a map of field names to terms
data Record m 
  = Record {
    recordTypeName :: Name,
    recordFields :: [Field m]}
  deriving (Eq, Ord, Read, Show)

_Record = (Name "hydra/core.Record")

_Record_typeName = (FieldName "typeName")

_Record_fields = (FieldName "fields")

-- A labeled record or union type
data RowType m 
  = RowType {
    rowTypeTypeName :: Name,
    rowTypeFields :: [FieldType m]}
  deriving (Eq, Ord, Read, Show)

_RowType = (Name "hydra/core.RowType")

_RowType_typeName = (FieldName "typeName")

_RowType_fields = (FieldName "fields")

-- A data term
data Term m 
  = TermAnnotated (Annotated (Term m) m)
  | TermApplication (Application m)
  | TermLiteral Literal
  | TermElement Name
  | TermFunction (Function m)
  | TermLet (Let m)
  | TermList [Term m]
  | TermMap (Map (Term m) (Term m))
  | TermNominal (Named m)
  | TermOptional (Maybe (Term m))
  | TermRecord (Record m)
  | TermSet (Set (Term m))
  | TermUnion (Union m)
  | TermVariable Variable
  deriving (Eq, Ord, Read, Show)

_Term = (Name "hydra/core.Term")

_Term_annotated = (FieldName "annotated")

_Term_application = (FieldName "application")

_Term_literal = (FieldName "literal")

_Term_element = (FieldName "element")

_Term_function = (FieldName "function")

_Term_let = (FieldName "let")

_Term_list = (FieldName "list")

_Term_map = (FieldName "map")

_Term_nominal = (FieldName "nominal")

_Term_optional = (FieldName "optional")

_Term_record = (FieldName "record")

_Term_set = (FieldName "set")

_Term_union = (FieldName "union")

_Term_variable = (FieldName "variable")

-- The identifier of a term expression constructor
data TermVariant 
  = TermVariantAnnotated 
  | TermVariantApplication 
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
  | TermVariantUnion 
  | TermVariantUniversal 
  | TermVariantVariable 
  deriving (Eq, Ord, Read, Show)

_TermVariant = (Name "hydra/core.TermVariant")

_TermVariant_annotated = (FieldName "annotated")

_TermVariant_application = (FieldName "application")

_TermVariant_element = (FieldName "element")

_TermVariant_function = (FieldName "function")

_TermVariant_let = (FieldName "let")

_TermVariant_list = (FieldName "list")

_TermVariant_literal = (FieldName "literal")

_TermVariant_map = (FieldName "map")

_TermVariant_nominal = (FieldName "nominal")

_TermVariant_optional = (FieldName "optional")

_TermVariant_record = (FieldName "record")

_TermVariant_set = (FieldName "set")

_TermVariant_union = (FieldName "union")

_TermVariant_universal = (FieldName "universal")

_TermVariant_variable = (FieldName "variable")

-- A data type
data Type m 
  = TypeAnnotated (Annotated (Type m) m)
  | TypeApplication (ApplicationType m)
  | TypeElement (Type m)
  | TypeFunction (FunctionType m)
  | TypeLambda (LambdaType m)
  | TypeList (Type m)
  | TypeLiteral LiteralType
  | TypeMap (MapType m)
  | TypeNominal Name
  | TypeOptional (Type m)
  | TypeRecord (RowType m)
  | TypeSet (Type m)
  | TypeUnion (RowType m)
  | TypeVariable VariableType
  deriving (Eq, Ord, Read, Show)

_Type = (Name "hydra/core.Type")

_Type_annotated = (FieldName "annotated")

_Type_application = (FieldName "application")

_Type_element = (FieldName "element")

_Type_function = (FieldName "function")

_Type_lambda = (FieldName "lambda")

_Type_list = (FieldName "list")

_Type_literal = (FieldName "literal")

_Type_map = (FieldName "map")

_Type_nominal = (FieldName "nominal")

_Type_optional = (FieldName "optional")

_Type_record = (FieldName "record")

_Type_set = (FieldName "set")

_Type_union = (FieldName "union")

_Type_variable = (FieldName "variable")

-- A type expression together with free type variables occurring in the expression
data TypeScheme m 
  = TypeScheme {
    typeSchemeVariables :: [VariableType],
    typeSchemeType :: (Type m)}
  deriving (Eq, Ord, Read, Show)

_TypeScheme = (Name "hydra/core.TypeScheme")

_TypeScheme_variables = (FieldName "variables")

_TypeScheme_type = (FieldName "type")

-- The identifier of a type constructor
data TypeVariant 
  = TypeVariantAnnotated 
  | TypeVariantApplication 
  | TypeVariantElement 
  | TypeVariantFunction 
  | TypeVariantLambda 
  | TypeVariantList 
  | TypeVariantLiteral 
  | TypeVariantMap 
  | TypeVariantNominal 
  | TypeVariantOptional 
  | TypeVariantRecord 
  | TypeVariantSet 
  | TypeVariantUnion 
  | TypeVariantVariable 
  deriving (Eq, Ord, Read, Show)

_TypeVariant = (Name "hydra/core.TypeVariant")

_TypeVariant_annotated = (FieldName "annotated")

_TypeVariant_application = (FieldName "application")

_TypeVariant_element = (FieldName "element")

_TypeVariant_function = (FieldName "function")

_TypeVariant_lambda = (FieldName "lambda")

_TypeVariant_list = (FieldName "list")

_TypeVariant_literal = (FieldName "literal")

_TypeVariant_map = (FieldName "map")

_TypeVariant_nominal = (FieldName "nominal")

_TypeVariant_optional = (FieldName "optional")

_TypeVariant_record = (FieldName "record")

_TypeVariant_set = (FieldName "set")

_TypeVariant_union = (FieldName "union")

_TypeVariant_variable = (FieldName "variable")

-- A type together with an instance of the type
data TypedTerm m 
  = TypedTerm {
    typedTermType :: (Type m),
    typedTermTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_TypedTerm = (Name "hydra/core.TypedTerm")

_TypedTerm_type = (FieldName "type")

_TypedTerm_term = (FieldName "term")

-- A symbol which stands in for a term
newtype Variable 
  = Variable {
    -- A symbol which stands in for a term
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = (Name "hydra/core.Variable")

-- A symbol which stands in for a type
newtype VariableType 
  = VariableType {
    -- A symbol which stands in for a type
    unVariableType :: String}
  deriving (Eq, Ord, Read, Show)

_VariableType = (Name "hydra/core.VariableType")

-- An instance of a union type; i.e. a string-indexed generalization of inl() or inr()
data Union m 
  = Union {
    unionTypeName :: Name,
    unionField :: (Field m)}
  deriving (Eq, Ord, Read, Show)

_Union = (Name "hydra/core.Union")

_Union_typeName = (FieldName "typeName")

_Union_field = (FieldName "field")