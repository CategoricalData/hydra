{-# LANGUAGE DeriveGeneric #-}
-- | The Hydra Core model
module Hydra.Core
  ( Application(..)
  , BooleanValue(..)
  , Comparison(..)
  , Expression(..)
  , Field(..)
  , FieldName
  , FieldType(..)
  , FloatType(..)
  , FloatValue(..)
  , Function(..)
  , FunctionType(..)
  , FunctionVariant(..)
  , IntegerType(..)
  , IntegerValue(..)
  , Lambda(..)
  , Let(..)
  , Literal(..)
  , LiteralType(..)
  , LiteralVariant(..)
  , MapType(..)
  , Meta(..)
  , Name
  , NominalTerm(..)
  , OptionalCases(..)
  , OptionalExpression(..)
  , Precision(..)
  , Term(..)
  , TermVariant(..)
  , Type(..)
  , TypeAbstraction(..)
  , TypeApplication(..)
  , TypeScheme(..)
  , TypeVariable
  , TypeVariant(..)
  , TypedTerm(..)
  , UniversalType(..)
  , Variable
  , _Application
  , _Application_argument
  , _Application_function
  , _BooleanValue
  , _BooleanValue_false
  , _BooleanValue_true
  , _Comparison
  , _Comparison_equalTo
  , _Comparison_greaterThan
  , _Comparison_lessThan
  , _Expression
  , _Expression_application
  , _Expression_element
  , _Expression_function
  , _Expression_let
  , _Expression_list
  , _Expression_literal
  , _Expression_map
  , _Expression_nominal
  , _Expression_optional
  , _Expression_record
  , _Expression_set
  , _Expression_typeAbstraction
  , _Expression_typeApplication
  , _Expression_union
  , _Expression_variable
  , _Field
  , _FieldName
  , _FieldType
  , _FieldType_name
  , _FieldType_type
  , _Field_name
  , _Field_term
  , _FloatType
  , _FloatType_bigfloat
  , _FloatType_float32
  , _FloatType_float64
  , _FloatValue
  , _FloatValue_bigfloat
  , _FloatValue_float32
  , _FloatValue_float64
  , _Function
  , _FunctionType
  , _FunctionType_codomain
  , _FunctionType_domain
  , _FunctionVariant
  , _FunctionVariant_cases
  , _FunctionVariant_compareTo
  , _FunctionVariant_data
  , _FunctionVariant_lambda
  , _FunctionVariant_optionalCases
  , _FunctionVariant_primitive
  , _FunctionVariant_projection
  , _Function_cases
  , _Function_compareTo
  , _Function_data
  , _Function_lambda
  , _Function_optionalCases
  , _Function_primitive
  , _Function_projection
  , _IntegerType
  , _IntegerType_bigint
  , _IntegerType_int16
  , _IntegerType_int32
  , _IntegerType_int64
  , _IntegerType_int8
  , _IntegerType_uint16
  , _IntegerType_uint32
  , _IntegerType_uint64
  , _IntegerType_uint8
  , _IntegerValue
  , _IntegerValue_bigint
  , _IntegerValue_int16
  , _IntegerValue_int32
  , _IntegerValue_int64
  , _IntegerValue_int8
  , _IntegerValue_uint16
  , _IntegerValue_uint32
  , _IntegerValue_uint64
  , _IntegerValue_uint8
  , _Lambda
  , _Lambda_body
  , _Lambda_parameter
  , _Let
  , _Let_environment
  , _Let_key
  , _Let_value
  , _Literal
  , _LiteralType
  , _LiteralType_binary
  , _LiteralType_boolean
  , _LiteralType_float
  , _LiteralType_integer
  , _LiteralType_string
  , _LiteralVariant
  , _LiteralVariant_binary
  , _LiteralVariant_boolean
  , _LiteralVariant_float
  , _LiteralVariant_integer
  , _LiteralVariant_string
  , _Literal_binary
  , _Literal_boolean
  , _Literal_float
  , _Literal_integer
  , _Literal_string
  , _MapType
  , _MapType_keys
  , _MapType_values
  , _Meta
  , _Meta_description
  , _Meta_type
  , _Name
  , _NominalTerm
  , _NominalTerm_term
  , _NominalTerm_typeName
  , _OptionalCases
  , _OptionalCases_just
  , _OptionalCases_nothing
  , _OptionalExpression
  , _OptionalExpression_just
  , _OptionalExpression_nothing
  , _Precision
  , _Precision_arbitrary
  , _Precision_bits
  , _Term
  , _TermVariant
  , _TermVariant_application
  , _TermVariant_element
  , _TermVariant_function
  , _TermVariant_let
  , _TermVariant_list
  , _TermVariant_literal
  , _TermVariant_map
  , _TermVariant_nominal
  , _TermVariant_optional
  , _TermVariant_record
  , _TermVariant_set
  , _TermVariant_typeAbstraction
  , _TermVariant_typeApplication
  , _TermVariant_union
  , _TermVariant_variable
  , _Term_data
  , _Term_meta
  , _Type
  , _TypeAbstraction
  , _TypeAbstraction_body
  , _TypeAbstraction_parameter
  , _TypeApplication
  , _TypeApplication_argument
  , _TypeApplication_function
  , _TypeScheme
  , _TypeScheme_type
  , _TypeScheme_variables
  , _TypeVariable
  , _TypeVariant
  , _TypeVariant_element
  , _TypeVariant_function
  , _TypeVariant_list
  , _TypeVariant_literal
  , _TypeVariant_map
  , _TypeVariant_nominal
  , _TypeVariant_optional
  , _TypeVariant_record
  , _TypeVariant_set
  , _TypeVariant_union
  , _TypeVariant_universal
  , _TypeVariant_variable
  , _Type_element
  , _Type_function
  , _Type_list
  , _Type_literal
  , _Type_map
  , _Type_nominal
  , _Type_optional
  , _Type_record
  , _Type_set
  , _Type_union
  , _Type_universal
  , _Type_variable
  , _TypedTerm
  , _TypedTerm_term
  , _TypedTerm_type
  , _UniversalType
  , _UniversalType_body
  , _UniversalType_variable
  , _Variable
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set

-- | A term which applies a function to an argument
data Application a
  = Application
    {-| The left-hand side of the application
        
        @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    { applicationFunction :: Term a
    {-| The right-hand side of the application
        
        @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    , applicationArgument :: Term a } deriving (Eq, Generic, Ord, Read, Show)

data BooleanValue
  = BooleanValueFalse
  | BooleanValueTrue deriving (Eq, Generic, Ord, Read, Show)

-- | An equality judgement: less than, equal to, or greater than
data Comparison
  = ComparisonLessThan
  | ComparisonEqualTo
  | ComparisonGreaterThan deriving (Eq, Generic, Ord, Read, Show)

data Expression a
  {-| A function application
      
      @type parameterized:
              genericType: hydra/core.Application
              parameters:
              - type:
                  variable: a
                variable: a -}
  = ExpressionApplication (Application a)
  {-| An element reference
      
      @type hydra/core.Name -}
  | ExpressionElement Name
  {-| A function term
      
      @type parameterized:
              genericType: hydra/core.Function
              parameters:
              - type:
                  variable: a
                variable: a -}
  | ExpressionFunction (Function a)
  {-| @type parameterized:
              genericType: hydra/core.Let
              parameters:
              - type:
                  variable: a
                variable: a -}
  | ExpressionLet (Let a)
  {-| A list
      
      @type list:
              parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
  | ExpressionList [Term a]
  {-| A literal value
      
      @type hydra/core.Literal -}
  | ExpressionLiteral Literal
  {-| A map of key terms to value terms
      
      @type map:
              keys:
                parameterized:
                  genericType: hydra/core.Term
                  parameters:
                  - type:
                      variable: a
                    variable: a
              values:
                parameterized:
                  genericType: hydra/core.Term
                  parameters:
                  - type:
                      variable: a
                    variable: a -}
  | ExpressionMap (Map (Term a) (Term a))
  {-| @type parameterized:
              genericType: hydra/core.NominalTerm
              parameters:
              - type:
                  variable: a
                variable: a -}
  | ExpressionNominal (NominalTerm a)
  {-| An optional value
      
      @type optional:
              parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
  | ExpressionOptional (Maybe (Term a))
  {-| A record, or labeled tuple
      
      @type list:
              parameterized:
                genericType: hydra/core.Field
                parameters:
                - type:
                    variable: a
                  variable: a -}
  | ExpressionRecord [Field a]
  {-| A set of terms
      
      @type set:
              parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
  | ExpressionSet (Set (Term a))
  {-| A type abstraction (generalization), which binds a type variable to a term
      
      @type parameterized:
              genericType: hydra/core.TypeAbstraction
              parameters:
              - type:
                  variable: a
                variable: a -}
  | ExpressionTypeAbstraction (TypeAbstraction a)
  {-| A type application (instantiation), which applies a term to a type
      
      @type parameterized:
              genericType: hydra/core.TypeApplication
              parameters:
              - type:
                  variable: a
                variable: a -}
  | ExpressionTypeApplication (TypeApplication a)
  {-| A union term, i.e. a string-indexed generalization of inl() or inr()
      
      @type parameterized:
              genericType: hydra/core.Field
              parameters:
              - type:
                  variable: a
                variable: a -}
  | ExpressionUnion (Field a)
  {-| A variable reference
      
      @type hydra/core.Variable -}
  | ExpressionVariable Variable deriving (Eq, Generic, Ord, Read, Show)

-- | A labeled term
data Field a
  = Field
    -- | @type hydra/core.FieldName
    { fieldName :: FieldName
    {-| @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    , fieldTerm :: Term a } deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type FieldName = String

data FieldType
  = FieldType
    -- | @type hydra/core.FieldName
    { fieldTypeName :: FieldName
    -- | @type hydra/core.Type
    , fieldTypeType :: Type } deriving (Eq, Generic, Ord, Read, Show)

data FloatType
  = FloatTypeBigfloat
  | FloatTypeFloat32
  | FloatTypeFloat64 deriving (Eq, Generic, Ord, Read, Show)

data FloatValue
  {-| @type float:
              precision: arbitrary -}
  = FloatValueBigfloat Double
  -- | @type float
  | FloatValueFloat32 Float
  {-| @type float:
              precision:
                bits: 64 -}
  | FloatValueFloat64 Double deriving (Eq, Generic, Ord, Read, Show)

data Function a
  {-| A case statement applied to a variant record, consisting of a function term
      for each alternative in the union
      
      @type list:
              parameterized:
                genericType: hydra/core.Field
                parameters:
                - type:
                    variable: a
                  variable: a -}
  = FunctionCases [Field a]
  {-| Compares a term with a given term of the same type, producing a Comparison
      
      @type parameterized:
              genericType: hydra/core.Term
              parameters:
              - type:
                  variable: a
                variable: a -}
  | FunctionCompareTo (Term a)
  -- | Hydra's delta function, which maps an element to its data term
  | FunctionData
  {-| A function abstraction (lambda)
      
      @type parameterized:
              genericType: hydra/core.Lambda
              parameters:
              - type:
                  variable: a
                variable: a -}
  | FunctionLambda (Lambda a)
  {-| Eliminator for optional terms
      
      @type parameterized:
              genericType: hydra/core.OptionalCases
              parameters:
              - type:
                  variable: a
                variable: a -}
  | FunctionOptionalCases (OptionalCases a)
  {-| A reference to a built-in (primitive) function
      
      @type hydra/core.Name -}
  | FunctionPrimitive Name
  {-| A projection of a field from a record
      
      @type hydra/core.FieldName -}
  | FunctionProjection FieldName deriving (Eq, Generic, Ord, Read, Show)

-- | A function type, also known as an arrow type
data FunctionType
  = FunctionType
    -- | @type hydra/core.Type
    { functionTypeDomain :: Type
    -- | @type hydra/core.Type
    , functionTypeCodomain :: Type } deriving (Eq, Generic, Ord, Read, Show)

data FunctionVariant
  = FunctionVariantCases
  | FunctionVariantCompareTo
  | FunctionVariantData
  | FunctionVariantLambda
  | FunctionVariantOptionalCases
  | FunctionVariantPrimitive
  | FunctionVariantProjection deriving (Eq, Generic, Ord, Read, Show)

data IntegerType
  = IntegerTypeBigint
  | IntegerTypeInt8
  | IntegerTypeInt16
  | IntegerTypeInt32
  | IntegerTypeInt64
  | IntegerTypeUint8
  | IntegerTypeUint16
  | IntegerTypeUint32
  | IntegerTypeUint64 deriving (Eq, Generic, Ord, Read, Show)

data IntegerValue
  {-| @type integer:
              precision: arbitrary -}
  = IntegerValueBigint Integer
  {-| @type integer:
              precision:
                bits: 8 -}
  | IntegerValueInt8 Integer
  {-| @type integer:
              precision:
                bits: 16 -}
  | IntegerValueInt16 Integer
  -- | @type integer
  | IntegerValueInt32 Int
  {-| @type integer:
              precision:
                bits: 64 -}
  | IntegerValueInt64 Int64
  {-| @type integer:
              precision:
                bits: 8
              signed: false -}
  | IntegerValueUint8 Integer
  {-| @type integer:
              precision:
                bits: 16
              signed: false -}
  | IntegerValueUint16 Integer
  {-| @type integer:
              signed: false -}
  | IntegerValueUint32 Int
  {-| @type integer:
              precision:
                bits: 64
              signed: false -}
  | IntegerValueUint64 Int64 deriving (Eq, Generic, Ord, Read, Show)

-- | A function abstraction (lambda)
data Lambda a
  = Lambda
    {-| The parameter of the lambda
        
        @type hydra/core.Variable -}
    { lambdaParameter :: Variable
    {-| The body of the lambda
        
        @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    , lambdaBody :: Term a } deriving (Eq, Generic, Ord, Read, Show)

-- | A 'let' binding
data Let a
  = Let
    -- | @type hydra/core.Variable
    { letKey :: Variable
    {-| @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    , letValue :: Term a
    {-| @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    , letEnvironment :: Term a } deriving (Eq, Generic, Ord, Read, Show)

-- | A term constant; an instance of a literal type
data Literal
  -- | @type binary
  = LiteralBinary String
  -- | @type hydra/core.BooleanValue
  | LiteralBoolean BooleanValue
  -- | @type hydra/core.FloatValue
  | LiteralFloat FloatValue
  -- | @type hydra/core.IntegerValue
  | LiteralInteger IntegerValue
  -- | @type string
  | LiteralString String deriving (Eq, Generic, Ord, Read, Show)

{-| Any of a fixed set of literal types, also called atomic types, base types,
    primitive types, or type constants
    
    @comments The so-called term constants, or valid values, of each literal type
    are unspecified -}
data LiteralType
  = LiteralTypeBinary
  | LiteralTypeBoolean
  -- | @type hydra/core.FloatType
  | LiteralTypeFloat FloatType
  -- | @type hydra/core.IntegerType
  | LiteralTypeInteger IntegerType
  | LiteralTypeString deriving (Eq, Generic, Ord, Read, Show)

data LiteralVariant
  = LiteralVariantBinary
  | LiteralVariantBoolean
  | LiteralVariantFloat
  | LiteralVariantInteger
  | LiteralVariantString deriving (Eq, Generic, Ord, Read, Show)

data MapType
  = MapType
    -- | @type hydra/core.Type
    { mapTypeKeys :: Type
    -- | @type hydra/core.Type
    , mapTypeValues :: Type } deriving (Eq, Generic, Ord, Read, Show)

-- | A built-in metadata container for terms
data Meta
  = Meta
    {-| An optional description associated with the term
        
        @type optional: string -}
    { metaDescription :: Maybe String
    {-| An optional type annotation associated with the term. This may be used as
        a hint to the type inferencer and/or to
        code generators.
        
        @type optional: hydra/core.Type -}
    , metaType :: Maybe Type } deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type Name = String

-- | A term annotated with a fixed, named type; an instance of a newtype
data NominalTerm a
  = NominalTerm
    -- | @type hydra/core.Name
    { nominalTermTypeName :: Name
    {-| @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    , nominalTermTerm :: Term a } deriving (Eq, Generic, Ord, Read, Show)

data OptionalCases a
  = OptionalCases
    {-| A term provided if the optional value is nothing
        
        @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    { optionalCasesNothing :: Term a
    {-| A function which is applied of the optional value is non-nothing
        
        @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    , optionalCasesJust :: Term a } deriving (Eq, Generic, Ord, Read, Show)

{-| An encoded optional value, for languages which do not natively support
    optionals -}
data OptionalExpression a
  {-| @type parameterized:
              genericType: hydra/core.Term
              parameters:
              - type:
                  variable: a
                variable: a -}
  = OptionalExpressionJust (Term a)
  | OptionalExpressionNothing deriving (Eq, Generic, Ord, Read, Show)

data Precision
  = PrecisionArbitrary
  -- | @type integer
  | PrecisionBits Int deriving (Eq, Generic, Ord, Read, Show)

data Term a
  = Term
    {-| @type parameterized:
                genericType: hydra/core.Expression
                parameters:
                - type:
                    variable: a
                  variable: a -}
    { termData :: Expression a
    -- | @type variable: a
    , termMeta :: a } deriving (Eq, Generic, Ord, Read, Show)

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
  | TermVariantVariable deriving (Eq, Generic, Ord, Read, Show)

data Type
  -- | @type hydra/core.Type
  = TypeElement Type
  -- | @type hydra/core.FunctionType
  | TypeFunction FunctionType
  -- | @type hydra/core.Type
  | TypeList Type
  -- | @type hydra/core.LiteralType
  | TypeLiteral LiteralType
  -- | @type hydra/core.MapType
  | TypeMap MapType
  -- | @type hydra/core.Name
  | TypeNominal Name
  -- | @type hydra/core.Type
  | TypeOptional Type
  -- | @type list: hydra/core.FieldType
  | TypeRecord [FieldType]
  -- | @type hydra/core.Type
  | TypeSet Type
  -- | @type list: hydra/core.FieldType
  | TypeUnion [FieldType]
  -- | @type hydra/core.UniversalType
  | TypeUniversal UniversalType
  -- | @type hydra/core.TypeVariable
  | TypeVariable TypeVariable deriving (Eq, Generic, Ord, Read, Show)

-- | A type abstraction (generalization), which binds a type variable to a term
data TypeAbstraction a
  = TypeAbstraction
    {-| The parameter of the abstraction
        
        @type hydra/core.TypeVariable -}
    { typeAbstractionParameter :: TypeVariable
    {-| The body of the abstraction
        
        @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    , typeAbstractionBody :: Term a } deriving (Eq, Generic, Ord, Read, Show)

-- | A type application (instantiation), which applies a term to a type
data TypeApplication a
  = TypeApplication
    {-| A term which is the left-hand side of the application
        
        @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    { typeApplicationFunction :: Term a
    {-| A type which is the right-hand side of the application
        
        @type hydra/core.Type -}
    , typeApplicationArgument :: Type } deriving (Eq, Generic, Ord, Read, Show)

data TypeScheme
  = TypeScheme
    -- | @type list: hydra/core.TypeVariable
    { typeSchemeVariables :: [TypeVariable]
    -- | @type hydra/core.Type
    , typeSchemeType :: Type } deriving (Eq, Generic, Ord, Read, Show)

{-| A symbol which stands in for a type
    
    @type string -}
type TypeVariable = String

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
  | TypeVariantVariable deriving (Eq, Generic, Ord, Read, Show)

data TypedTerm a
  = TypedTerm
    -- | @type hydra/core.Type
    { typedTermType :: Type
    {-| @type parameterized:
                genericType: hydra/core.Term
                parameters:
                - type:
                    variable: a
                  variable: a -}
    , typedTermTerm :: Term a } deriving (Eq, Generic, Ord, Read, Show)

-- | A universally quantified ('forall') type, parameterized by a type variable
data UniversalType
  = UniversalType
    -- | @type hydra/core.TypeVariable
    { universalTypeVariable :: TypeVariable
    -- | @type hydra/core.Type
    , universalTypeBody :: Type } deriving (Eq, Generic, Ord, Read, Show)

{-| A symbol which stands in for a term
    
    @type string -}
type Variable = String

_Application = "hydra/core.Application" :: String
_Application_argument = "argument" :: String
_Application_function = "function" :: String
_BooleanValue = "hydra/core.BooleanValue" :: String
_BooleanValue_false = "false" :: String
_BooleanValue_true = "true" :: String
_Comparison = "hydra/core.Comparison" :: String
_Comparison_equalTo = "equalTo" :: String
_Comparison_greaterThan = "greaterThan" :: String
_Comparison_lessThan = "lessThan" :: String
_Expression = "hydra/core.Expression" :: String
_Expression_application = "application" :: String
_Expression_element = "element" :: String
_Expression_function = "function" :: String
_Expression_let = "let" :: String
_Expression_list = "list" :: String
_Expression_literal = "literal" :: String
_Expression_map = "map" :: String
_Expression_nominal = "nominal" :: String
_Expression_optional = "optional" :: String
_Expression_record = "record" :: String
_Expression_set = "set" :: String
_Expression_typeAbstraction = "typeAbstraction" :: String
_Expression_typeApplication = "typeApplication" :: String
_Expression_union = "union" :: String
_Expression_variable = "variable" :: String
_Field = "hydra/core.Field" :: String
_FieldName = "hydra/core.FieldName" :: String
_FieldType = "hydra/core.FieldType" :: String
_FieldType_name = "name" :: String
_FieldType_type = "type" :: String
_Field_name = "name" :: String
_Field_term = "term" :: String
_FloatType = "hydra/core.FloatType" :: String
_FloatType_bigfloat = "bigfloat" :: String
_FloatType_float32 = "float32" :: String
_FloatType_float64 = "float64" :: String
_FloatValue = "hydra/core.FloatValue" :: String
_FloatValue_bigfloat = "bigfloat" :: String
_FloatValue_float32 = "float32" :: String
_FloatValue_float64 = "float64" :: String
_Function = "hydra/core.Function" :: String
_FunctionType = "hydra/core.FunctionType" :: String
_FunctionType_codomain = "codomain" :: String
_FunctionType_domain = "domain" :: String
_FunctionVariant = "hydra/core.FunctionVariant" :: String
_FunctionVariant_cases = "cases" :: String
_FunctionVariant_compareTo = "compareTo" :: String
_FunctionVariant_data = "data" :: String
_FunctionVariant_lambda = "lambda" :: String
_FunctionVariant_optionalCases = "optionalCases" :: String
_FunctionVariant_primitive = "primitive" :: String
_FunctionVariant_projection = "projection" :: String
_Function_cases = "cases" :: String
_Function_compareTo = "compareTo" :: String
_Function_data = "data" :: String
_Function_lambda = "lambda" :: String
_Function_optionalCases = "optionalCases" :: String
_Function_primitive = "primitive" :: String
_Function_projection = "projection" :: String
_IntegerType = "hydra/core.IntegerType" :: String
_IntegerType_bigint = "bigint" :: String
_IntegerType_int16 = "int16" :: String
_IntegerType_int32 = "int32" :: String
_IntegerType_int64 = "int64" :: String
_IntegerType_int8 = "int8" :: String
_IntegerType_uint16 = "uint16" :: String
_IntegerType_uint32 = "uint32" :: String
_IntegerType_uint64 = "uint64" :: String
_IntegerType_uint8 = "uint8" :: String
_IntegerValue = "hydra/core.IntegerValue" :: String
_IntegerValue_bigint = "bigint" :: String
_IntegerValue_int16 = "int16" :: String
_IntegerValue_int32 = "int32" :: String
_IntegerValue_int64 = "int64" :: String
_IntegerValue_int8 = "int8" :: String
_IntegerValue_uint16 = "uint16" :: String
_IntegerValue_uint32 = "uint32" :: String
_IntegerValue_uint64 = "uint64" :: String
_IntegerValue_uint8 = "uint8" :: String
_Lambda = "hydra/core.Lambda" :: String
_Lambda_body = "body" :: String
_Lambda_parameter = "parameter" :: String
_Let = "hydra/core.Let" :: String
_Let_environment = "environment" :: String
_Let_key = "key" :: String
_Let_value = "value" :: String
_Literal = "hydra/core.Literal" :: String
_LiteralType = "hydra/core.LiteralType" :: String
_LiteralType_binary = "binary" :: String
_LiteralType_boolean = "boolean" :: String
_LiteralType_float = "float" :: String
_LiteralType_integer = "integer" :: String
_LiteralType_string = "string" :: String
_LiteralVariant = "hydra/core.LiteralVariant" :: String
_LiteralVariant_binary = "binary" :: String
_LiteralVariant_boolean = "boolean" :: String
_LiteralVariant_float = "float" :: String
_LiteralVariant_integer = "integer" :: String
_LiteralVariant_string = "string" :: String
_Literal_binary = "binary" :: String
_Literal_boolean = "boolean" :: String
_Literal_float = "float" :: String
_Literal_integer = "integer" :: String
_Literal_string = "string" :: String
_MapType = "hydra/core.MapType" :: String
_MapType_keys = "keys" :: String
_MapType_values = "values" :: String
_Meta = "hydra/core.Meta" :: String
_Meta_description = "description" :: String
_Meta_type = "type" :: String
_Name = "hydra/core.Name" :: String
_NominalTerm = "hydra/core.NominalTerm" :: String
_NominalTerm_term = "term" :: String
_NominalTerm_typeName = "typeName" :: String
_OptionalCases = "hydra/core.OptionalCases" :: String
_OptionalCases_just = "just" :: String
_OptionalCases_nothing = "nothing" :: String
_OptionalExpression = "hydra/core.OptionalExpression" :: String
_OptionalExpression_just = "just" :: String
_OptionalExpression_nothing = "nothing" :: String
_Precision = "hydra/core.Precision" :: String
_Precision_arbitrary = "arbitrary" :: String
_Precision_bits = "bits" :: String
_Term = "hydra/core.Term" :: String
_TermVariant = "hydra/core.TermVariant" :: String
_TermVariant_application = "application" :: String
_TermVariant_element = "element" :: String
_TermVariant_function = "function" :: String
_TermVariant_let = "let" :: String
_TermVariant_list = "list" :: String
_TermVariant_literal = "literal" :: String
_TermVariant_map = "map" :: String
_TermVariant_nominal = "nominal" :: String
_TermVariant_optional = "optional" :: String
_TermVariant_record = "record" :: String
_TermVariant_set = "set" :: String
_TermVariant_typeAbstraction = "typeAbstraction" :: String
_TermVariant_typeApplication = "typeApplication" :: String
_TermVariant_union = "union" :: String
_TermVariant_variable = "variable" :: String
_Term_data = "data" :: String
_Term_meta = "meta" :: String
_Type = "hydra/core.Type" :: String
_TypeAbstraction = "hydra/core.TypeAbstraction" :: String
_TypeAbstraction_body = "body" :: String
_TypeAbstraction_parameter = "parameter" :: String
_TypeApplication = "hydra/core.TypeApplication" :: String
_TypeApplication_argument = "argument" :: String
_TypeApplication_function = "function" :: String
_TypeScheme = "hydra/core.TypeScheme" :: String
_TypeScheme_type = "type" :: String
_TypeScheme_variables = "variables" :: String
_TypeVariable = "hydra/core.TypeVariable" :: String
_TypeVariant = "hydra/core.TypeVariant" :: String
_TypeVariant_element = "element" :: String
_TypeVariant_function = "function" :: String
_TypeVariant_list = "list" :: String
_TypeVariant_literal = "literal" :: String
_TypeVariant_map = "map" :: String
_TypeVariant_nominal = "nominal" :: String
_TypeVariant_optional = "optional" :: String
_TypeVariant_record = "record" :: String
_TypeVariant_set = "set" :: String
_TypeVariant_union = "union" :: String
_TypeVariant_universal = "universal" :: String
_TypeVariant_variable = "variable" :: String
_Type_element = "element" :: String
_Type_function = "function" :: String
_Type_list = "list" :: String
_Type_literal = "literal" :: String
_Type_map = "map" :: String
_Type_nominal = "nominal" :: String
_Type_optional = "optional" :: String
_Type_record = "record" :: String
_Type_set = "set" :: String
_Type_union = "union" :: String
_Type_universal = "universal" :: String
_Type_variable = "variable" :: String
_TypedTerm = "hydra/core.TypedTerm" :: String
_TypedTerm_term = "term" :: String
_TypedTerm_type = "type" :: String
_UniversalType = "hydra/core.UniversalType" :: String
_UniversalType_body = "body" :: String
_UniversalType_variable = "variable" :: String
_Variable = "hydra/core.Variable" :: String
