-- | Phantom-typed term DSL for the hydra.lib.logic library

{-# LANGUAGE FlexibleContexts #-}

module Hydra.Dsl.Meta.Lib.Logic where

import Hydra.Typed
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Compute the logical AND of two boolean values.
and :: TypedTerm Bool -> TypedTerm Bool -> TypedTerm Bool
and = primitive2 _logic_and

-- | Compute a conditional expression.
ifElse :: (AsTerm c Bool, AsTerm t a, AsTerm e a) => c -> t -> e -> TypedTerm a
ifElse cond t e = primitive3 _logic_ifElse (asTerm cond) (asTerm t) (asTerm e)

-- | Compute the logical NOT of a boolean value.
not :: TypedTerm Bool -> TypedTerm Bool
not = primitive1 _logic_not

-- | Compute the logical OR of two boolean values.
or :: TypedTerm Bool -> TypedTerm Bool -> TypedTerm Bool
or = primitive2 _logic_or

----------------------------------------
-- Helpers which are not primitives

-- | Fold a list of booleans with logical AND, returning True for an empty list.
ands :: TypedTerm [Bool] -> TypedTerm Bool
ands terms = fold (primitive _logic_and) @@ true @@ terms

-- | Fold a list of booleans with logical OR, returning False for an empty list.
ors :: TypedTerm [Bool] -> TypedTerm Bool
ors terms = fold (primitive _logic_or) @@ false @@ terms
