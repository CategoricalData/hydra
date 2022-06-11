module Hydra.Core where

import Data.Map
import Data.Set

-- A term which applies a function to an argument
data Application m 
  = Application {
    applicationFunction :: (Term m),
    applicationArgument :: (Term m)}
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
    fieldTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Field = (Name "hydra/core.Field")

_Field_name = (FieldName "name")

_Field_term = (FieldName "term")

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
    lambdaParameter :: Variable,
    lambdaBody :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Lambda = (Name "hydra/core.Lambda")

_Lambda_parameter = (FieldName "parameter")

_Lambda_body = (FieldName "body")

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
    metaAnnotations :: (Map String (Term Meta))}
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
    namedTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_Named = (Name "hydra/core.Named")

_Named_typeName = (FieldName "typeName")

_Named_term = (FieldName "term")

-- A case statement for matching optional terms
data OptionalCases m 
  = OptionalCases {
    optionalCasesNothing :: (Term m),
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

-- A data term
data Term m 
  = Term {
    termExpr :: (TermExpr m),
    termMeta :: m}
  deriving (Eq, Ord, Read, Show)

_Term = (Name "hydra/core.Term")

_Term_expr = (FieldName "expr")

_Term_meta = (FieldName "meta")

-- A term expression
data TermExpr m 
  = TermExprApplication (Application m)
  | TermExprLiteral Literal
  | TermExprElement Name
  | TermExprFunction (Function m)
  | TermExprLet (Let m)
  | TermExprList [Term m]
  | TermExprMap (Map (Term m) (Term m))
  | TermExprNominal (Named m)
  | TermExprOptional (Maybe (Term m))
  | TermExprRecord [Field m]
  | TermExprSet (Set (Term m))
  | TermExprTypeAbstraction (TypeAbstraction m)
  | TermExprTypeApplication (TypeApplication m)
  | TermExprUnion (Field m)
  | TermExprVariable Variable
  deriving (Eq, Ord, Read, Show)

_TermExpr = (Name "hydra/core.TermExpr")

_TermExpr_application = (FieldName "application")

_TermExpr_literal = (FieldName "literal")

_TermExpr_element = (FieldName "element")

_TermExpr_function = (FieldName "function")

_TermExpr_let = (FieldName "let")

_TermExpr_list = (FieldName "list")

_TermExpr_map = (FieldName "map")

_TermExpr_nominal = (FieldName "nominal")

_TermExpr_optional = (FieldName "optional")

_TermExpr_record = (FieldName "record")

_TermExpr_set = (FieldName "set")

_TermExpr_typeAbstraction = (FieldName "typeAbstraction")

_TermExpr_typeApplication = (FieldName "typeApplication")

_TermExpr_union = (FieldName "union")

_TermExpr_variable = (FieldName "variable")

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
  | TermVariantUniversal
  | TermVariantVariable 
  deriving (Eq, Ord, Read, Show)

_TermVariant = (Name "hydra/core.TermVariant")

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

_TermVariant_typeAbstraction = (FieldName "typeAbstraction")

_TermVariant_typeApplication = (FieldName "typeApplication")

_TermVariant_union = (FieldName "union")

_TermVariant_variable = (FieldName "variable")

-- A data type
data Type m 
  = Type {
    typeExpr :: (TypeExpr m),
    typeMeta :: m}
  deriving (Eq, Ord, Read, Show)

_Type = (Name "hydra/core.Type")

_Type_expr = (FieldName "expr")

_Type_meta = (FieldName "meta")

-- A type abstraction (generalization), which binds a type variable to a term
data TypeAbstraction m 
  = TypeAbstraction {
    typeAbstractionParameter :: TypeVariable,
    typeAbstractionBody :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_TypeAbstraction = (Name "hydra/core.TypeAbstraction")

_TypeAbstraction_parameter = (FieldName "parameter")

_TypeAbstraction_body = (FieldName "body")

-- A type application (instantiation), which applies a term to a type
data TypeApplication m 
  = TypeApplication {
    typeApplicationFunction :: (Term m),
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

_TypeExpr = (Name "hydra/core.TypeExpr")

_TypeExpr_element = (FieldName "element")

_TypeExpr_function = (FieldName "function")

_TypeExpr_list = (FieldName "list")

_TypeExpr_literal = (FieldName "literal")

_TypeExpr_map = (FieldName "map")

_TypeExpr_nominal = (FieldName "nominal")

_TypeExpr_optional = (FieldName "optional")

_TypeExpr_record = (FieldName "record")

_TypeExpr_set = (FieldName "set")

_TypeExpr_union = (FieldName "union")

_TypeExpr_universal = (FieldName "universal")

_TypeExpr_variable = (FieldName "variable")

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
data TypedTerm m 
  = TypedTerm {
    typedTermType :: (Type m),
    typedTermTerm :: (Term m)}
  deriving (Eq, Ord, Read, Show)

_TypedTerm = (Name "hydra/core.TypedTerm")

_TypedTerm_type = (FieldName "type")

_TypedTerm_term = (FieldName "term")

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