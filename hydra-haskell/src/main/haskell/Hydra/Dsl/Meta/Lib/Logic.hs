-- | Phantom-typed term DSL for the hydra.lib.logic library

module Hydra.Dsl.Meta.Lib.Logic where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


and :: TTerm Bool -> TTerm Bool -> TTerm Bool
and = primitive2 _logic_and

ifElse :: TTerm Bool -> TTerm a -> TTerm a -> TTerm a
ifElse = primitive3 _logic_ifElse

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
