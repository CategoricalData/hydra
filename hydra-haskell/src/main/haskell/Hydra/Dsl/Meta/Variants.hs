-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Variants module.
-- Adds Haskell enum -> TTerm converter functions.

module Hydra.Dsl.Meta.Variants (
  module Hydra.Dsl.Variants,
  module Hydra.Dsl.Meta.Variants,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Dsl.Variants


-- | Convert a Haskell EliminationVariant to a TTerm
eliminationVariant :: EliminationVariant -> TTerm EliminationVariant
eliminationVariant v = injectUnit _EliminationVariant $ case v of
  EliminationVariantRecord -> _EliminationVariant_record
  EliminationVariantUnion -> _EliminationVariant_union
  EliminationVariantWrap -> _EliminationVariant_wrap

-- | Convert a Haskell FunctionVariant to a TTerm
functionVariant :: FunctionVariant -> TTerm FunctionVariant
functionVariant v = injectUnit _FunctionVariant $ case v of
  FunctionVariantElimination -> _FunctionVariant_elimination
  FunctionVariantLambda -> _FunctionVariant_lambda
  FunctionVariantPrimitive -> _FunctionVariant_primitive

-- | Convert a Haskell LiteralVariant to a TTerm
literalVariant :: LiteralVariant -> TTerm LiteralVariant
literalVariant v = injectUnit _LiteralVariant $ case v of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

-- | Convert a Haskell TermVariant to a TTerm
termVariant :: TermVariant -> TTerm TermVariant
termVariant v = injectUnit _TermVariant $ case v of
  TermVariantAnnotated -> _TermVariant_annotated
  TermVariantApplication -> _TermVariant_application
  TermVariantEither -> _TermVariant_either
  TermVariantFunction -> _TermVariant_function
  TermVariantLet -> _TermVariant_let
  TermVariantList -> _TermVariant_list
  TermVariantLiteral -> _TermVariant_literal
  TermVariantMap -> _TermVariant_map
  TermVariantMaybe -> _TermVariant_maybe
  TermVariantPair -> _TermVariant_pair
  TermVariantRecord -> _TermVariant_record
  TermVariantSet -> _TermVariant_set
  TermVariantTypeLambda -> _TermVariant_typeLambda
  TermVariantTypeApplication -> _TermVariant_typeApplication
  TermVariantUnion -> _TermVariant_union
  TermVariantUnit -> _TermVariant_unit
  TermVariantVariable -> _TermVariant_variable
  TermVariantWrap -> _TermVariant_wrap

-- | Convert a Haskell TypeVariant to a TTerm
typeVariant :: TypeVariant -> TTerm TypeVariant
typeVariant v = injectUnit _TypeVariant $ case v of
  TypeVariantAnnotated -> _TypeVariant_annotated
  TypeVariantApplication -> _TypeVariant_application
  TypeVariantEither -> _TypeVariant_either
  TypeVariantFunction -> _TypeVariant_function
  TypeVariantForall -> _TypeVariant_forall
  TypeVariantList -> _TypeVariant_list
  TypeVariantLiteral -> _TypeVariant_literal
  TypeVariantMap -> _TypeVariant_map
  TypeVariantMaybe -> _TypeVariant_maybe
  TypeVariantPair -> _TypeVariant_pair
  TypeVariantRecord -> _TypeVariant_record
  TypeVariantSet -> _TypeVariant_set
  TypeVariantUnion -> _TypeVariant_union
  TypeVariantUnit -> _TypeVariant_unit
  TypeVariantVariable -> _TypeVariant_variable
  TypeVariantWrap -> _TypeVariant_wrap
