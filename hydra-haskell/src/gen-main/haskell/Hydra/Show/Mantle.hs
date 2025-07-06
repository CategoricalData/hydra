-- | String representations of hydra.mantle types

module Hydra.Show.Mantle where

import qualified Hydra.Mantle as Mantle
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Show a term variant as a string
termVariant :: (Mantle.TermVariant -> String)
termVariant x = case x of
  Mantle.TermVariantAnnotated -> "annotated"
  Mantle.TermVariantApplication -> "application"
  Mantle.TermVariantFunction -> "function"
  Mantle.TermVariantLet -> "let"
  Mantle.TermVariantList -> "list"
  Mantle.TermVariantLiteral -> "literal"
  Mantle.TermVariantMap -> "map"
  Mantle.TermVariantOptional -> "optional"
  Mantle.TermVariantProduct -> "product"
  Mantle.TermVariantRecord -> "record"
  Mantle.TermVariantSet -> "set"
  Mantle.TermVariantSum -> "sum"
  Mantle.TermVariantTypeAbstraction -> "typeAbstraction"
  Mantle.TermVariantTypeApplication -> "typeApplication"
  Mantle.TermVariantUnion -> "union"
  Mantle.TermVariantUnit -> "unit"
  Mantle.TermVariantVariable -> "variable"
  Mantle.TermVariantWrap -> "wrap"

-- | Show a type variant as a string
typeVariant :: (Mantle.TypeVariant -> String)
typeVariant x = case x of
  Mantle.TypeVariantAnnotated -> "annotated"
  Mantle.TypeVariantApplication -> "application"
  Mantle.TypeVariantForall -> "forall"
  Mantle.TypeVariantFunction -> "function"
  Mantle.TypeVariantList -> "list"
  Mantle.TypeVariantLiteral -> "literal"
  Mantle.TypeVariantMap -> "map"
  Mantle.TypeVariantOptional -> "optional"
  Mantle.TypeVariantProduct -> "product"
  Mantle.TypeVariantRecord -> "record"
  Mantle.TypeVariantSet -> "set"
  Mantle.TypeVariantSum -> "sum"
  Mantle.TypeVariantUnion -> "union"
  Mantle.TypeVariantUnit -> "unit"
  Mantle.TypeVariantVariable -> "variable"
  Mantle.TypeVariantWrap -> "wrap"
