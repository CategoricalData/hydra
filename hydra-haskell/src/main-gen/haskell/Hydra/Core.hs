{-# LANGUAGE DeriveGeneric #-}
-- | The Hydra Core model (in progress)
module Hydra.Core
  ( Application(..)
  , AtomicType(..)
  , AtomicValue(..)
  , AtomicVariant(..)
  , BooleanValue(..)
  , CaseStatement(..)
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
  , Name
  , Term(..)
  , TermVariant(..)
  , Type(..)
  , TypeVariant(..)
  , Variable
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

data CaseStatement
  = CaseStatement
    {-| A handler for each alternative in a union type. The term of each case
        must be function-typed.
        
        @type list: hydra/core.Field -}
    { caseStatementCases :: [Field]
    {-| A convenience which allows certain "don't care" cases to be omitted. The
        result is a term which does not otherwise
        depend on the variant value.
        
        @type hydra/core.Term -}
    , caseStatementDefaultEsc :: Term } deriving (Eq, Generic, Ord, Read, Show)

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

-- | @type string
type Name = String

data Term
  {-| A function application
      
      @type hydra/core.Application -}
  = TermApplication Application
  {-| An atomic value
      
      @type hydra/core.AtomicValue -}
  | TermAtomic AtomicValue
  {-| A case statement applied to a variant record
      
      @type hydra/core.CaseStatement -}
  | TermCases CaseStatement
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
  {-| A projection of a field from a record
      
      @type hydra/core.FieldName -}
  | TermProjection FieldName
  {-| A record, or labeled tuple
      
      @type list: hydra/core.Field -}
  | TermRecord [Field]
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
  | TermVariantProjection
  | TermVariantRecord
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
  -- | @type hydra/core.Name
  | TypeNominal Name
  -- | @type list: hydra/core.FieldType
  | TypeRecord [FieldType]
  -- | @type list: hydra/core.FieldType
  | TypeUnion [FieldType] deriving (Eq, Generic, Ord, Read, Show)

data TypeVariant
  = TypeVariantAtomic
  | TypeVariantElement
  | TypeVariantFunction
  | TypeVariantList
  | TypeVariantNominal
  | TypeVariantRecord
  | TypeVariantUnion deriving (Eq, Generic, Ord, Read, Show)

{-| A symbol which stands in for a term
    
    @type string -}
type Variable = String
