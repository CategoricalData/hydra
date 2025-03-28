-- | A set of types which supplement hydra.core with variants and accessors. Currently contains miscellaneous additional types including CaseConvention and Either.

module Hydra.Mantle where

import qualified Hydra.Core as Core
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

-- | A disjoint union between a 'left' type and a 'right' type
data Either_ a b = 
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

-- | A function which maps from a term to a particular immediate subterm
data TermAccessor = 
  TermAccessorAnnotatedSubject  |
  TermAccessorApplicationFunction  |
  TermAccessorApplicationArgument  |
  TermAccessorLambdaBody  |
  TermAccessorUnionCasesDefault  |
  TermAccessorUnionCasesBranch Core.Name |
  TermAccessorLetEnvironment  |
  TermAccessorLetBinding Core.Name |
  TermAccessorListElement Int |
  TermAccessorMapKey Int |
  TermAccessorMapValue Int |
  TermAccessorOptionalTerm  |
  TermAccessorProductTerm Int |
  TermAccessorRecordField Core.Name |
  TermAccessorSetElement Int |
  TermAccessorSumTerm  |
  TermAccessorTypeAbstractionBody  |
  TermAccessorTypeApplicationTerm  |
  TermAccessorTypedTerm  |
  TermAccessorInjectionTerm  |
  TermAccessorWrappedTerm 
  deriving (Eq, Ord, Read, Show)

_TermAccessor = (Core.Name "hydra.mantle.TermAccessor")

_TermAccessor_annotatedSubject = (Core.Name "annotatedSubject")

_TermAccessor_applicationFunction = (Core.Name "applicationFunction")

_TermAccessor_applicationArgument = (Core.Name "applicationArgument")

_TermAccessor_lambdaBody = (Core.Name "lambdaBody")

_TermAccessor_unionCasesDefault = (Core.Name "unionCasesDefault")

_TermAccessor_unionCasesBranch = (Core.Name "unionCasesBranch")

_TermAccessor_letEnvironment = (Core.Name "letEnvironment")

_TermAccessor_letBinding = (Core.Name "letBinding")

_TermAccessor_listElement = (Core.Name "listElement")

_TermAccessor_mapKey = (Core.Name "mapKey")

_TermAccessor_mapValue = (Core.Name "mapValue")

_TermAccessor_optionalTerm = (Core.Name "optionalTerm")

_TermAccessor_productTerm = (Core.Name "productTerm")

_TermAccessor_recordField = (Core.Name "recordField")

_TermAccessor_setElement = (Core.Name "setElement")

_TermAccessor_sumTerm = (Core.Name "sumTerm")

_TermAccessor_typeAbstractionBody = (Core.Name "typeAbstractionBody")

_TermAccessor_typeApplicationTerm = (Core.Name "typeApplicationTerm")

_TermAccessor_typedTerm = (Core.Name "typedTerm")

_TermAccessor_injectionTerm = (Core.Name "injectionTerm")

_TermAccessor_wrappedTerm = (Core.Name "wrappedTerm")

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
  TermVariantTypeAbstraction  |
  TermVariantTypeApplication  |
  TermVariantTyped  |
  TermVariantUnion  |
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

_TermVariant_typeAbstraction = (Core.Name "typeAbstraction")

_TermVariant_typeApplication = (Core.Name "typeApplication")

_TermVariant_typed = (Core.Name "typed")

_TermVariant_union = (Core.Name "union")

_TermVariant_variable = (Core.Name "variable")

_TermVariant_wrap = (Core.Name "wrap")

-- | The identifier of a type constructor
data TypeVariant = 
  TypeVariantAnnotated  |
  TypeVariantApplication  |
  TypeVariantFunction  |
  TypeVariantLambda  |
  TypeVariantList  |
  TypeVariantLiteral  |
  TypeVariantMap  |
  TypeVariantOptional  |
  TypeVariantProduct  |
  TypeVariantRecord  |
  TypeVariantSet  |
  TypeVariantSum  |
  TypeVariantUnion  |
  TypeVariantVariable  |
  TypeVariantWrap 
  deriving (Eq, Ord, Read, Show)

_TypeVariant = (Core.Name "hydra.mantle.TypeVariant")

_TypeVariant_annotated = (Core.Name "annotated")

_TypeVariant_application = (Core.Name "application")

_TypeVariant_function = (Core.Name "function")

_TypeVariant_lambda = (Core.Name "lambda")

_TypeVariant_list = (Core.Name "list")

_TypeVariant_literal = (Core.Name "literal")

_TypeVariant_map = (Core.Name "map")

_TypeVariant_optional = (Core.Name "optional")

_TypeVariant_product = (Core.Name "product")

_TypeVariant_record = (Core.Name "record")

_TypeVariant_set = (Core.Name "set")

_TypeVariant_sum = (Core.Name "sum")

_TypeVariant_union = (Core.Name "union")

_TypeVariant_variable = (Core.Name "variable")

_TypeVariant_wrap = (Core.Name "wrap")
