{-# LANGUAGE FlexibleContexts #-}

-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Core module.
-- Re-exports all generated DSL functions and adds AsTerm-flexible overrides
-- for functions commonly called with TBinding arguments.

module Hydra.Dsl.Meta.Core (
  module Hydra.Dsl.Core,
  module Hydra.Dsl.Meta.Core,
  nameLift,
) where

import Hydra.Kernel
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms (nameLift, var, (~>))
import Hydra.Dsl.Core hiding (binding, injection, typeVariable)
import qualified Hydra.Dsl.Core as Gen

-- For helpers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic as Logic

import qualified Data.Map as M
import Prelude hiding (map, product)


-- | AsTerm-flexible overrides of generated functions.
-- These allow passing TBinding values where TTerm is expected.

binding :: AsTerm t Term => TTerm Name -> t -> TTerm (Maybe TypeScheme) -> TTerm Binding
binding n t ts = Gen.binding n (asTerm t) ts

injection :: AsTerm t Name => t -> TTerm Field -> TTerm Injection
injection n f = Gen.injection (asTerm n) f

typeVariable :: AsTerm t Name => t -> TTerm Type
typeVariable n = Gen.typeVariable (asTerm n)


-- | Non-standard helpers (used in kernel source modules)

equalName_ :: TTerm Name -> TTerm Name -> TTerm Bool
equalName_ left right = Equality.equal (Gen.unName left) (Gen.unName right)

equalNameList_ :: TTerm [Name] -> TTerm [Name] -> TTerm Bool
equalNameList_ lefts rights = Logic.and
  (Equality.equal (Lists.length lefts) (Lists.length rights))
  (Logic.ands $ Lists.zipWith equalName lefts rights)
  where
    equalName = "left" ~> "right" ~> Equality.equal
      (Gen.unName (var "left" :: TTerm Name))
      (Gen.unName (var "right" :: TTerm Name))

false :: TTerm Term
false = termLiteral $ literalBoolean $ TTerm $ TermLiteral $ LiteralBoolean False
