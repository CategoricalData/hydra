-- | Phantom-typed term DSL for the hydra.lib.logic library

{-# LANGUAGE FlexibleContexts #-}

module Hydra.Dsl.Meta.Lib.Logic where

import Hydra.Phantoms
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


and :: TTerm Bool -> TTerm Bool -> TTerm Bool
and = primitive2 _logic_and

-- | Conditional expression: ifElse condition thenBranch elseBranch
-- Accepts TTerm or TBinding for condition and branches (via AsTerm)
ifElse :: (AsTerm c Bool, AsTerm t a, AsTerm e a) => c -> t -> e -> TTerm a
ifElse cond t e = primitive3 _logic_ifElse (asTerm cond) (asTerm t) (asTerm e)

not :: TTerm Bool -> TTerm Bool
not = primitive1 _logic_not

or :: TTerm Bool -> TTerm Bool -> TTerm Bool
or = primitive2 _logic_or

----------------------------------------
-- Helpers which are not primitives

ands :: TTerm [Bool] -> TTerm Bool
ands terms = fold (primitive _logic_and) @@ true @@ terms

ors :: TTerm [Bool] -> TTerm Bool
ors terms = fold (primitive _logic_or) @@ false @@ terms
