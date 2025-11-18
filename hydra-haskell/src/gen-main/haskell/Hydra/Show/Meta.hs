-- | String representations of hydra.meta types

module Hydra.Show.Meta where

import qualified Hydra.Meta as Meta
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Show a term variant as a string
termVariant :: (Meta.TermVariant -> String)
termVariant x = case x of
  Meta.TermVariantAnnotated -> "annotated"
  Meta.TermVariantApplication -> "application"
  Meta.TermVariantEither -> "either"
  Meta.TermVariantFunction -> "function"
  Meta.TermVariantLet -> "let"
  Meta.TermVariantList -> "list"
  Meta.TermVariantLiteral -> "literal"
  Meta.TermVariantMap -> "map"
  Meta.TermVariantMaybe -> "maybe"
  Meta.TermVariantPair -> "pair"
  Meta.TermVariantProduct -> "product"
  Meta.TermVariantRecord -> "record"
  Meta.TermVariantSet -> "set"
  Meta.TermVariantSum -> "sum"
  Meta.TermVariantTypeLambda -> "typeLambda"
  Meta.TermVariantTypeApplication -> "typeApplication"
  Meta.TermVariantUnion -> "union"
  Meta.TermVariantUnit -> "unit"
  Meta.TermVariantVariable -> "variable"
  Meta.TermVariantWrap -> "wrap"

-- | Show a type variant as a string
typeVariant :: (Meta.TypeVariant -> String)
typeVariant x = case x of
  Meta.TypeVariantAnnotated -> "annotated"
  Meta.TypeVariantApplication -> "application"
  Meta.TypeVariantEither -> "either"
  Meta.TypeVariantForall -> "forall"
  Meta.TypeVariantFunction -> "function"
  Meta.TypeVariantList -> "list"
  Meta.TypeVariantLiteral -> "literal"
  Meta.TypeVariantMap -> "map"
  Meta.TypeVariantMaybe -> "maybe"
  Meta.TypeVariantPair -> "pair"
  Meta.TypeVariantProduct -> "product"
  Meta.TypeVariantRecord -> "record"
  Meta.TypeVariantSet -> "set"
  Meta.TypeVariantSum -> "sum"
  Meta.TypeVariantUnion -> "union"
  Meta.TypeVariantUnit -> "unit"
  Meta.TypeVariantVariable -> "variable"
  Meta.TypeVariantWrap -> "wrap"
