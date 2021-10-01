{-# LANGUAGE DeriveGeneric #-}
-- | The Hydra Core model (in progress)
module Hydra.Core
  ( Application(..)
  , AtomicType(..)
  , AtomicValue(..)
  , AtomicVariant(..)
  , BooleanValue(..)
  , Comparison(..)
  , Field(..)
  , FieldName
  , FieldType(..)
  , FloatType(..)
  , FloatValue(..)
  , FloatVariant(..)
  , FunctionType(..)
  , IntegerType(..)
  , IntegerValue(..)
  , IntegerVariant(..)
  , Lambda(..)
  , MapType(..)
  , Name
  , Precision(..)
  , Term(..)
  , TermVariant(..)
  , Type(..)
  , TypeVariant(..)
  , Variable
  , _Application
  , _Application_argument
  , _Application_function
  , _AtomicType
  , _AtomicType_binary
  , _AtomicType_boolean
  , _AtomicType_float
  , _AtomicType_integer
  , _AtomicType_string
  , _AtomicValue
  , _AtomicValue_binary
  , _AtomicValue_boolean
  , _AtomicValue_float
  , _AtomicValue_integer
  , _AtomicValue_string
  , _AtomicVariant
  , _AtomicVariant_binary
  , _AtomicVariant_boolean
  , _AtomicVariant_float
  , _AtomicVariant_integer
  , _AtomicVariant_string
  , _BooleanValue
  , _BooleanValue_false
  , _BooleanValue_true
  , _Comparison
  , _Comparison_equalTo
  , _Comparison_greaterThan
  , _Comparison_lessThan
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
  , _FloatVariant
  , _FloatVariant_bigfloat
  , _FloatVariant_float32
  , _FloatVariant_float64
  , _FunctionType
  , _FunctionType_codomain
  , _FunctionType_domain
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
  , _IntegerVariant
  , _IntegerVariant_bigint
  , _IntegerVariant_int16
  , _IntegerVariant_int32
  , _IntegerVariant_int64
  , _IntegerVariant_int8
  , _IntegerVariant_uint16
  , _IntegerVariant_uint32
  , _IntegerVariant_uint64
  , _IntegerVariant_uint8
  , _Lambda
  , _Lambda_body
  , _Lambda_parameter
  , _MapType
  , _MapType_keys
  , _MapType_values
  , _Name
  , _Precision
  , _Precision_arbitrary
  , _Precision_bits
  , _Term
  , _TermVariant
  , _TermVariant_application
  , _TermVariant_atomic
  , _TermVariant_cases
  , _TermVariant_compareTo
  , _TermVariant_data
  , _TermVariant_element
  , _TermVariant_function
  , _TermVariant_lambda
  , _TermVariant_list
  , _TermVariant_map
  , _TermVariant_projection
  , _TermVariant_record
  , _TermVariant_set
  , _TermVariant_union
  , _TermVariant_variable
  , _Term_application
  , _Term_atomic
  , _Term_cases
  , _Term_compareTo
  , _Term_data
  , _Term_element
  , _Term_function
  , _Term_lambda
  , _Term_list
  , _Term_map
  , _Term_projection
  , _Term_record
  , _Term_set
  , _Term_union
  , _Term_variable
  , _Type
  , _TypeVariant
  , _TypeVariant_atomic
  , _TypeVariant_element
  , _TypeVariant_function
  , _TypeVariant_list
  , _TypeVariant_map
  , _TypeVariant_nominal
  , _TypeVariant_record
  , _TypeVariant_set
  , _TypeVariant_union
  , _Type_atomic
  , _Type_element
  , _Type_function
  , _Type_list
  , _Type_map
  , _Type_nominal
  , _Type_record
  , _Type_set
  , _Type_union
  , _Variable
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set

-- | A term which applies a function to an argument
data Application
  = Application
    -- | @type hydra/core.Term
    { applicationFunction :: Term
    -- | @type hydra/core.Term
    , applicationArgument :: Term } deriving (Eq, Generic, Ord, Read, Show)

{-| Any of a fixed set of atomic types, also called base types, primitive types,
    or type constants
    
    @comments The so-called term constants, or valid values, of each atomic type
    are unspecified -}
data AtomicType
  = AtomicTypeBinary
  | AtomicTypeBoolean
  -- | @type hydra/core.FloatType
  | AtomicTypeFloat FloatType
  -- | @type hydra/core.IntegerType
  | AtomicTypeInteger IntegerType
  | AtomicTypeString deriving (Eq, Generic, Ord, Read, Show)

-- | A term constant; an instance of an atomic type
data AtomicValue
  -- | @type binary
  = AtomicValueBinary String
  -- | @type hydra/core.BooleanValue
  | AtomicValueBoolean BooleanValue
  -- | @type hydra/core.FloatValue
  | AtomicValueFloat FloatValue
  -- | @type hydra/core.IntegerValue
  | AtomicValueInteger IntegerValue
  -- | @type string
  | AtomicValueString String deriving (Eq, Generic, Ord, Read, Show)

data AtomicVariant
  = AtomicVariantBinary
  | AtomicVariantBoolean
  | AtomicVariantFloat
  | AtomicVariantInteger
  | AtomicVariantString deriving (Eq, Generic, Ord, Read, Show)

data BooleanValue
  = BooleanValueFalse
  | BooleanValueTrue deriving (Eq, Generic, Ord, Read, Show)

-- | An equality judgement: less than, equal to, or greater than
data Comparison
  = ComparisonLessThan
  | ComparisonEqualTo
  | ComparisonGreaterThan deriving (Eq, Generic, Ord, Read, Show)

-- | A labeled term
data Field
  = Field
    -- | @type hydra/core.FieldName
    { fieldName :: FieldName
    -- | @type hydra/core.Term
    , fieldTerm :: Term } deriving (Eq, Generic, Ord, Read, Show)

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

data FloatVariant
  = FloatVariantBigfloat
  | FloatVariantFloat32
  | FloatVariantFloat64 deriving (Eq, Generic, Ord, Read, Show)

-- | A function type, also known as an arrow type
data FunctionType
  = FunctionType
    -- | @type hydra/core.Type
    { functionTypeDomain :: Type
    -- | @type hydra/core.Type
    , functionTypeCodomain :: Type } deriving (Eq, Generic, Ord, Read, Show)

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

data IntegerVariant
  = IntegerVariantBigint
  | IntegerVariantInt8
  | IntegerVariantInt16
  | IntegerVariantInt32
  | IntegerVariantInt64
  | IntegerVariantUint8
  | IntegerVariantUint16
  | IntegerVariantUint32
  | IntegerVariantUint64 deriving (Eq, Generic, Ord, Read, Show)

-- | A function abstraction (lambda)
data Lambda
  = Lambda
    {-| The parameter of the lambda
        
        @type hydra/core.Variable -}
    { lambdaParameter :: Variable
    {-| The body of the lambda
        
        @type hydra/core.Term -}
    , lambdaBody :: Term } deriving (Eq, Generic, Ord, Read, Show)

data MapType
  = MapType
    -- | @type hydra/core.Type
    { mapTypeKeys :: Type
    -- | @type hydra/core.Type
    , mapTypeValues :: Type } deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type Name = String

data Precision
  = PrecisionArbitrary
  -- | @type integer
  | PrecisionBits Int deriving (Eq, Generic, Ord, Read, Show)

data Term
  {-| A function application
      
      @type hydra/core.Application -}
  = TermApplication Application
  {-| An atomic value
      
      @type hydra/core.AtomicValue -}
  | TermAtomic AtomicValue
  {-| A case statement applied to a variant record, consisting of a function term
      for each alternative in the union
      
      @type list: hydra/core.Field -}
  | TermCases [Field]
  {-| Compares a term with a given term of the same type, producing a Comparison
      
      @type hydra/core.Term -}
  | TermCompareTo Term
  -- | Hydra's delta function, which maps an element to its data term
  | TermData
  {-| An element reference
      
      @type hydra/core.Name -}
  | TermElement Name
  {-| A reference to a built-in function
      
      @type hydra/core.Name -}
  | TermFunction Name
  {-| A function abstraction (lambda)
      
      @type hydra/core.Lambda -}
  | TermLambda Lambda
  {-| A list
      
      @type list: hydra/core.Term -}
  | TermList [Term]
  {-| A map of key terms to value terms
      
      @type map:
              keys: hydra/core.Term
              values: hydra/core.Term -}
  | TermMap (Map Term Term)
  {-| A projection of a field from a record
      
      @type hydra/core.FieldName -}
  | TermProjection FieldName
  {-| A record, or labeled tuple
      
      @type list: hydra/core.Field -}
  | TermRecord [Field]
  {-| A set of terms
      
      @type set: hydra/core.Term -}
  | TermSet (Set Term)
  {-| A union term, i.e. a generalization of inl() or inr()
      
      @type hydra/core.Field -}
  | TermUnion Field
  {-| A variable reference
      
      @type hydra/core.Variable -}
  | TermVariable Variable deriving (Eq, Generic, Ord, Read, Show)

data TermVariant
  = TermVariantApplication
  | TermVariantAtomic
  | TermVariantCases
  | TermVariantCompareTo
  | TermVariantData
  | TermVariantElement
  | TermVariantFunction
  | TermVariantLambda
  | TermVariantList
  | TermVariantMap
  | TermVariantProjection
  | TermVariantRecord
  | TermVariantSet
  | TermVariantUnion
  | TermVariantVariable deriving (Eq, Generic, Ord, Read, Show)

data Type
  -- | @type hydra/core.AtomicType
  = TypeAtomic AtomicType
  -- | @type hydra/core.Type
  | TypeElement Type
  -- | @type hydra/core.FunctionType
  | TypeFunction FunctionType
  -- | @type hydra/core.Type
  | TypeList Type
  -- | @type hydra/core.MapType
  | TypeMap MapType
  -- | @type hydra/core.Name
  | TypeNominal Name
  -- | @type list: hydra/core.FieldType
  | TypeRecord [FieldType]
  -- | @type hydra/core.Type
  | TypeSet Type
  -- | @type list: hydra/core.FieldType
  | TypeUnion [FieldType] deriving (Eq, Generic, Ord, Read, Show)

data TypeVariant
  = TypeVariantAtomic
  | TypeVariantElement
  | TypeVariantFunction
  | TypeVariantList
  | TypeVariantMap
  | TypeVariantNominal
  | TypeVariantRecord
  | TypeVariantSet
  | TypeVariantUnion deriving (Eq, Generic, Ord, Read, Show)

{-| A symbol which stands in for a term
    
    @type string -}
type Variable = String

_Application = "hydra/core.Application" :: String
_Application_argument = "argument" :: String
_Application_function = "function" :: String
_AtomicType = "hydra/core.AtomicType" :: String
_AtomicType_binary = "binary" :: String
_AtomicType_boolean = "boolean" :: String
_AtomicType_float = "float" :: String
_AtomicType_integer = "integer" :: String
_AtomicType_string = "string" :: String
_AtomicValue = "hydra/core.AtomicValue" :: String
_AtomicValue_binary = "binary" :: String
_AtomicValue_boolean = "boolean" :: String
_AtomicValue_float = "float" :: String
_AtomicValue_integer = "integer" :: String
_AtomicValue_string = "string" :: String
_AtomicVariant = "hydra/core.AtomicVariant" :: String
_AtomicVariant_binary = "binary" :: String
_AtomicVariant_boolean = "boolean" :: String
_AtomicVariant_float = "float" :: String
_AtomicVariant_integer = "integer" :: String
_AtomicVariant_string = "string" :: String
_BooleanValue = "hydra/core.BooleanValue" :: String
_BooleanValue_false = "false" :: String
_BooleanValue_true = "true" :: String
_Comparison = "hydra/core.Comparison" :: String
_Comparison_equalTo = "equalTo" :: String
_Comparison_greaterThan = "greaterThan" :: String
_Comparison_lessThan = "lessThan" :: String
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
_FloatVariant = "hydra/core.FloatVariant" :: String
_FloatVariant_bigfloat = "bigfloat" :: String
_FloatVariant_float32 = "float32" :: String
_FloatVariant_float64 = "float64" :: String
_FunctionType = "hydra/core.FunctionType" :: String
_FunctionType_codomain = "codomain" :: String
_FunctionType_domain = "domain" :: String
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
_IntegerVariant = "hydra/core.IntegerVariant" :: String
_IntegerVariant_bigint = "bigint" :: String
_IntegerVariant_int16 = "int16" :: String
_IntegerVariant_int32 = "int32" :: String
_IntegerVariant_int64 = "int64" :: String
_IntegerVariant_int8 = "int8" :: String
_IntegerVariant_uint16 = "uint16" :: String
_IntegerVariant_uint32 = "uint32" :: String
_IntegerVariant_uint64 = "uint64" :: String
_IntegerVariant_uint8 = "uint8" :: String
_Lambda = "hydra/core.Lambda" :: String
_Lambda_body = "body" :: String
_Lambda_parameter = "parameter" :: String
_MapType = "hydra/core.MapType" :: String
_MapType_keys = "keys" :: String
_MapType_values = "values" :: String
_Name = "hydra/core.Name" :: String
_Precision = "hydra/core.Precision" :: String
_Precision_arbitrary = "arbitrary" :: String
_Precision_bits = "bits" :: String
_Term = "hydra/core.Term" :: String
_TermVariant = "hydra/core.TermVariant" :: String
_TermVariant_application = "application" :: String
_TermVariant_atomic = "atomic" :: String
_TermVariant_cases = "cases" :: String
_TermVariant_compareTo = "compareTo" :: String
_TermVariant_data = "data" :: String
_TermVariant_element = "element" :: String
_TermVariant_function = "function" :: String
_TermVariant_lambda = "lambda" :: String
_TermVariant_list = "list" :: String
_TermVariant_map = "map" :: String
_TermVariant_projection = "projection" :: String
_TermVariant_record = "record" :: String
_TermVariant_set = "set" :: String
_TermVariant_union = "union" :: String
_TermVariant_variable = "variable" :: String
_Term_application = "application" :: String
_Term_atomic = "atomic" :: String
_Term_cases = "cases" :: String
_Term_compareTo = "compareTo" :: String
_Term_data = "data" :: String
_Term_element = "element" :: String
_Term_function = "function" :: String
_Term_lambda = "lambda" :: String
_Term_list = "list" :: String
_Term_map = "map" :: String
_Term_projection = "projection" :: String
_Term_record = "record" :: String
_Term_set = "set" :: String
_Term_union = "union" :: String
_Term_variable = "variable" :: String
_Type = "hydra/core.Type" :: String
_TypeVariant = "hydra/core.TypeVariant" :: String
_TypeVariant_atomic = "atomic" :: String
_TypeVariant_element = "element" :: String
_TypeVariant_function = "function" :: String
_TypeVariant_list = "list" :: String
_TypeVariant_map = "map" :: String
_TypeVariant_nominal = "nominal" :: String
_TypeVariant_record = "record" :: String
_TypeVariant_set = "set" :: String
_TypeVariant_union = "union" :: String
_Type_atomic = "atomic" :: String
_Type_element = "element" :: String
_Type_function = "function" :: String
_Type_list = "list" :: String
_Type_map = "map" :: String
_Type_nominal = "nominal" :: String
_Type_record = "record" :: String
_Type_set = "set" :: String
_Type_union = "union" :: String
_Variable = "hydra/core.Variable" :: String
