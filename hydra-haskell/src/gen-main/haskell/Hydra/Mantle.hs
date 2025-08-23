-- | A set of types which supplement hydra.core, but are not referenced by hydra.core.

module Hydra.Mantle where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data CaseConvention = 
  CaseConventionCamel  |
  CaseConventionPascal  |
  CaseConventionLowerSnake  |
  CaseConventionUpperSnake 
  deriving (Eq, Ord, Read, Show)

_CaseConvention = (Core.Name "hydra.mantle.CaseConvention")

_CaseConvention_camel = (Core.Name "camel")

_CaseConvention_pascal = (Core.Name "pascal")

_CaseConvention_lowerSnake = (Core.Name "lowerSnake")

_CaseConvention_upperSnake = (Core.Name "upperSnake")

-- | An equality judgement: less than, equal to, or greater than
data Comparison = 
  ComparisonLessThan  |
  ComparisonEqualTo  |
  ComparisonGreaterThan 
  deriving (Eq, Ord, Read, Show)

_Comparison = (Core.Name "hydra.mantle.Comparison")

_Comparison_lessThan = (Core.Name "lessThan")

_Comparison_equalTo = (Core.Name "equalTo")

_Comparison_greaterThan = (Core.Name "greaterThan")

-- | A disjoint union between a 'left' type and a 'right' type
data Either a b = 
  EitherLeft a |
  EitherRight b
  deriving (Eq, Ord, Read, Show)

_Either = (Core.Name "hydra.mantle.Either")

_Either_left = (Core.Name "left")

_Either_right = (Core.Name "right")

-- | The identifier of an elimination constructor
data EliminationVariant = 
  EliminationVariantProduct  |
  EliminationVariantRecord  |
  EliminationVariantUnion  |
  EliminationVariantWrap 
  deriving (Eq, Ord, Read, Show)

_EliminationVariant = (Core.Name "hydra.mantle.EliminationVariant")

_EliminationVariant_product = (Core.Name "product")

_EliminationVariant_record = (Core.Name "record")

_EliminationVariant_union = (Core.Name "union")

_EliminationVariant_wrap = (Core.Name "wrap")

-- | The identifier of a function constructor
data FunctionVariant = 
  FunctionVariantElimination  |
  FunctionVariantLambda  |
  FunctionVariantPrimitive 
  deriving (Eq, Ord, Read, Show)

_FunctionVariant = (Core.Name "hydra.mantle.FunctionVariant")

_FunctionVariant_elimination = (Core.Name "elimination")

_FunctionVariant_lambda = (Core.Name "lambda")

_FunctionVariant_primitive = (Core.Name "primitive")

-- | The identifier of a literal constructor
data LiteralVariant = 
  LiteralVariantBinary  |
  LiteralVariantBoolean  |
  LiteralVariantFloat  |
  LiteralVariantInteger  |
  LiteralVariantString 
  deriving (Eq, Ord, Read, Show)

_LiteralVariant = (Core.Name "hydra.mantle.LiteralVariant")

_LiteralVariant_binary = (Core.Name "binary")

_LiteralVariant_boolean = (Core.Name "boolean")

_LiteralVariant_float = (Core.Name "float")

_LiteralVariant_integer = (Core.Name "integer")

_LiteralVariant_string = (Core.Name "string")

-- | Numeric precision: arbitrary precision, or precision to a specified number of bits
data Precision = 
  PrecisionArbitrary  |
  PrecisionBits Int
  deriving (Eq, Ord, Read, Show)

_Precision = (Core.Name "hydra.mantle.Precision")

_Precision_arbitrary = (Core.Name "arbitrary")

_Precision_bits = (Core.Name "bits")

-- | The identifier of a term expression constructor
data TermVariant = 
  TermVariantAnnotated  |
  TermVariantApplication  |
  TermVariantFunction  |
  TermVariantLet  |
  TermVariantList  |
  TermVariantLiteral  |
  TermVariantMap  |
  TermVariantOptional  |
  TermVariantProduct  |
  TermVariantRecord  |
  TermVariantSet  |
  TermVariantSum  |
  TermVariantTypeLambda  |
  TermVariantTypeApplication  |
  TermVariantUnion  |
  TermVariantUnit  |
  TermVariantVariable  |
  TermVariantWrap 
  deriving (Eq, Ord, Read, Show)

_TermVariant = (Core.Name "hydra.mantle.TermVariant")

_TermVariant_annotated = (Core.Name "annotated")

_TermVariant_application = (Core.Name "application")

_TermVariant_function = (Core.Name "function")

_TermVariant_let = (Core.Name "let")

_TermVariant_list = (Core.Name "list")

_TermVariant_literal = (Core.Name "literal")

_TermVariant_map = (Core.Name "map")

_TermVariant_optional = (Core.Name "optional")

_TermVariant_product = (Core.Name "product")

_TermVariant_record = (Core.Name "record")

_TermVariant_set = (Core.Name "set")

_TermVariant_sum = (Core.Name "sum")

_TermVariant_typeLambda = (Core.Name "typeLambda")

_TermVariant_typeApplication = (Core.Name "typeApplication")

_TermVariant_union = (Core.Name "union")

_TermVariant_unit = (Core.Name "unit")

_TermVariant_variable = (Core.Name "variable")

_TermVariant_wrap = (Core.Name "wrap")

-- | Any of a small number of built-in type classes
data TypeClass = 
  TypeClassEquality  |
  TypeClassOrdering 
  deriving (Eq, Ord, Read, Show)

_TypeClass = (Core.Name "hydra.mantle.TypeClass")

_TypeClass_equality = (Core.Name "equality")

_TypeClass_ordering = (Core.Name "ordering")

-- | The identifier of a type constructor
data TypeVariant = 
  TypeVariantAnnotated  |
  TypeVariantApplication  |
  TypeVariantForall  |
  TypeVariantFunction  |
  TypeVariantList  |
  TypeVariantLiteral  |
  TypeVariantMap  |
  TypeVariantOptional  |
  TypeVariantProduct  |
  TypeVariantRecord  |
  TypeVariantSet  |
  TypeVariantSum  |
  TypeVariantUnion  |
  TypeVariantUnit  |
  TypeVariantVariable  |
  TypeVariantWrap 
  deriving (Eq, Ord, Read, Show)

_TypeVariant = (Core.Name "hydra.mantle.TypeVariant")

_TypeVariant_annotated = (Core.Name "annotated")

_TypeVariant_application = (Core.Name "application")

_TypeVariant_forall = (Core.Name "forall")

_TypeVariant_function = (Core.Name "function")

_TypeVariant_list = (Core.Name "list")

_TypeVariant_literal = (Core.Name "literal")

_TypeVariant_map = (Core.Name "map")

_TypeVariant_optional = (Core.Name "optional")

_TypeVariant_product = (Core.Name "product")

_TypeVariant_record = (Core.Name "record")

_TypeVariant_set = (Core.Name "set")

_TypeVariant_sum = (Core.Name "sum")

_TypeVariant_union = (Core.Name "union")

_TypeVariant_unit = (Core.Name "unit")

_TypeVariant_variable = (Core.Name "variable")

_TypeVariant_wrap = (Core.Name "wrap")
