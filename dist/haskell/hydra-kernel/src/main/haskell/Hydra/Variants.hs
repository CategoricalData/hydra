-- Note: this is an automatically generated file. Do not edit.

-- | Variant types which describe the structure of Hydra core types and terms.

module Hydra.Variants where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | The identifier of an elimination constructor (legacy)
data EliminationVariant =
  EliminationVariantRecord  |
  EliminationVariantUnion  |
  EliminationVariantWrap
  deriving (Eq, Ord, Read, Show)

_EliminationVariant = Core.Name "hydra.variants.EliminationVariant"

_EliminationVariant_record = Core.Name "record"

_EliminationVariant_union = Core.Name "union"

_EliminationVariant_wrap = Core.Name "wrap"

-- | The identifier of a function constructor (legacy)
data FunctionVariant =
  FunctionVariantElimination  |
  FunctionVariantLambda
  deriving (Eq, Ord, Read, Show)

_FunctionVariant = Core.Name "hydra.variants.FunctionVariant"

_FunctionVariant_elimination = Core.Name "elimination"

_FunctionVariant_lambda = Core.Name "lambda"

-- | The identifier of a literal constructor
data LiteralVariant =
  LiteralVariantBinary  |
  LiteralVariantBoolean  |
  LiteralVariantFloat  |
  LiteralVariantInteger  |
  LiteralVariantString
  deriving (Eq, Ord, Read, Show)

_LiteralVariant = Core.Name "hydra.variants.LiteralVariant"

_LiteralVariant_binary = Core.Name "binary"

_LiteralVariant_boolean = Core.Name "boolean"

_LiteralVariant_float = Core.Name "float"

_LiteralVariant_integer = Core.Name "integer"

_LiteralVariant_string = Core.Name "string"

-- | The identifier of a term expression constructor
data TermVariant =
  TermVariantAnnotated  |
  TermVariantApplication  |
  TermVariantCases  |
  TermVariantEither  |
  TermVariantLambda  |
  TermVariantLet  |
  TermVariantList  |
  TermVariantLiteral  |
  TermVariantMap  |
  TermVariantMaybe  |
  TermVariantPair  |
  TermVariantProject  |
  TermVariantRecord  |
  TermVariantSet  |
  TermVariantTypeApplication  |
  TermVariantTypeLambda  |
  TermVariantUnion  |
  TermVariantUnit  |
  TermVariantUnwrap  |
  TermVariantVariable  |
  TermVariantWrap
  deriving (Eq, Ord, Read, Show)

_TermVariant = Core.Name "hydra.variants.TermVariant"

_TermVariant_annotated = Core.Name "annotated"

_TermVariant_application = Core.Name "application"

_TermVariant_cases = Core.Name "cases"

_TermVariant_either = Core.Name "either"

_TermVariant_lambda = Core.Name "lambda"

_TermVariant_let = Core.Name "let"

_TermVariant_list = Core.Name "list"

_TermVariant_literal = Core.Name "literal"

_TermVariant_map = Core.Name "map"

_TermVariant_maybe = Core.Name "maybe"

_TermVariant_pair = Core.Name "pair"

_TermVariant_project = Core.Name "project"

_TermVariant_record = Core.Name "record"

_TermVariant_set = Core.Name "set"

_TermVariant_typeApplication = Core.Name "typeApplication"

_TermVariant_typeLambda = Core.Name "typeLambda"

_TermVariant_union = Core.Name "union"

_TermVariant_unit = Core.Name "unit"

_TermVariant_unwrap = Core.Name "unwrap"

_TermVariant_variable = Core.Name "variable"

_TermVariant_wrap = Core.Name "wrap"

-- | The identifier of a type constructor
data TypeVariant =
  TypeVariantAnnotated  |
  TypeVariantApplication  |
  TypeVariantEither  |
  TypeVariantForall  |
  TypeVariantFunction  |
  TypeVariantList  |
  TypeVariantLiteral  |
  TypeVariantMap  |
  TypeVariantMaybe  |
  TypeVariantPair  |
  TypeVariantRecord  |
  TypeVariantSet  |
  TypeVariantUnion  |
  TypeVariantUnit  |
  TypeVariantVariable  |
  TypeVariantVoid  |
  TypeVariantWrap
  deriving (Eq, Ord, Read, Show)

_TypeVariant = Core.Name "hydra.variants.TypeVariant"

_TypeVariant_annotated = Core.Name "annotated"

_TypeVariant_application = Core.Name "application"

_TypeVariant_either = Core.Name "either"

_TypeVariant_forall = Core.Name "forall"

_TypeVariant_function = Core.Name "function"

_TypeVariant_list = Core.Name "list"

_TypeVariant_literal = Core.Name "literal"

_TypeVariant_map = Core.Name "map"

_TypeVariant_maybe = Core.Name "maybe"

_TypeVariant_pair = Core.Name "pair"

_TypeVariant_record = Core.Name "record"

_TypeVariant_set = Core.Name "set"

_TypeVariant_union = Core.Name "union"

_TypeVariant_unit = Core.Name "unit"

_TypeVariant_variable = Core.Name "variable"

_TypeVariant_void = Core.Name "void"

_TypeVariant_wrap = Core.Name "wrap"
