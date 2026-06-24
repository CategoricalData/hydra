{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Core module.
-- Re-exports all generated DSL functions and adds AsTerm-flexible overrides
-- for functions commonly called with TypedBinding arguments.

module Hydra.Overlay.Haskell.Dsl.Typed.Core (
  module Hydra.Dsl.Core,
  module Hydra.Overlay.Haskell.Dsl.Typed.Core,
  nameLift,
) where

import Hydra.Kernel
import Hydra.Overlay.Haskell.AsTerm
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms (nameLift, var, (~>))
import Hydra.Dsl.Core hiding (binding, injection, typeVariable)
import qualified Hydra.Dsl.Core as Gen

-- For helpers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Logic as Logic

import qualified Data.Map as M
import Prelude hiding (map, product)


-- | AsTerm-flexible overrides of generated functions.
-- These allow passing TypedBinding values where TypedTerm is expected.

binding :: AsTerm t Term => TypedTerm Name -> t -> TypedTerm (Maybe TypeScheme) -> TypedTerm Binding
binding n t ts = Gen.binding n (asTerm t) ts

equalNameList_ :: TypedTerm [Name] -> TypedTerm [Name] -> TypedTerm Bool
equalNameList_ lefts rights = Logic.and
  (Equality.equal (Lists.length lefts) (Lists.length rights))
  (ands $ Lists.zipWith equalName lefts rights)
  where
    equalName = "left" ~> "right" ~> Equality.equal
      (Gen.unName (var "left" :: TypedTerm Name))
      (Gen.unName (var "right" :: TypedTerm Name))
    -- Fold a list of booleans with logical AND (True for the empty list).
    -- The generated Hydra.Dsl.Lib.Logic has no 'ands' helper, so fold inline.
    ands bools = Lists.foldl ("acc" ~> "x" ~> Logic.and (var "acc") (var "x")) true bools
    true = TypedTerm (TermLiteral (LiteralBoolean True)) :: TypedTerm Bool

equalName_ :: TypedTerm Name -> TypedTerm Name -> TypedTerm Bool
equalName_ left right = Equality.equal (Gen.unName left) (Gen.unName right)

false :: TypedTerm Term
false = termLiteral $ literalBoolean $ TypedTerm $ TermLiteral $ LiteralBoolean False

injection :: AsTerm t Name => t -> TypedTerm Field -> TypedTerm Injection
injection n f = Gen.injection (asTerm n) f

typeVariable :: AsTerm t Name => t -> TypedTerm Type
typeVariable n = Gen.typeVariable (asTerm n)


-- | Non-standard helpers (used in kernel source modules)
