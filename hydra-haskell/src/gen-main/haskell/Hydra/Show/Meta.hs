-- Note: this is an automatically generated file. Do not edit.

-- | String representations of hydra.meta types

module Hydra.Show.Meta where

import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Show a term variant as a string
termVariant :: (Variants.TermVariant -> String)
termVariant x = case x of
  Variants.TermVariantAnnotated -> "annotated"
  Variants.TermVariantApplication -> "application"
  Variants.TermVariantEither -> "either"
  Variants.TermVariantFunction -> "function"
  Variants.TermVariantLet -> "let"
  Variants.TermVariantList -> "list"
  Variants.TermVariantLiteral -> "literal"
  Variants.TermVariantMap -> "map"
  Variants.TermVariantMaybe -> "maybe"
  Variants.TermVariantPair -> "pair"
  Variants.TermVariantProduct -> "product"
  Variants.TermVariantRecord -> "record"
  Variants.TermVariantSet -> "set"
  Variants.TermVariantSum -> "sum"
  Variants.TermVariantTypeLambda -> "typeLambda"
  Variants.TermVariantTypeApplication -> "typeApplication"
  Variants.TermVariantUnion -> "union"
  Variants.TermVariantUnit -> "unit"
  Variants.TermVariantVariable -> "variable"
  Variants.TermVariantWrap -> "wrap"

-- | Show a type variant as a string
typeVariant :: (Variants.TypeVariant -> String)
typeVariant x = case x of
  Variants.TypeVariantAnnotated -> "annotated"
  Variants.TypeVariantApplication -> "application"
  Variants.TypeVariantEither -> "either"
  Variants.TypeVariantForall -> "forall"
  Variants.TypeVariantFunction -> "function"
  Variants.TypeVariantList -> "list"
  Variants.TypeVariantLiteral -> "literal"
  Variants.TypeVariantMap -> "map"
  Variants.TypeVariantMaybe -> "maybe"
  Variants.TypeVariantPair -> "pair"
  Variants.TypeVariantProduct -> "product"
  Variants.TypeVariantRecord -> "record"
  Variants.TypeVariantSet -> "set"
  Variants.TypeVariantSum -> "sum"
  Variants.TypeVariantUnion -> "union"
  Variants.TypeVariantUnit -> "unit"
  Variants.TypeVariantVariable -> "variable"
  Variants.TypeVariantWrap -> "wrap"
