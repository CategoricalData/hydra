module Hydra.Dsl.Lib.Logic where

import Hydra.Core
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Base


and :: TTerm Bool -> TTerm Bool -> TTerm Bool
and = primitive2 _logic_and

ifElse :: TTerm Bool -> TTerm a -> TTerm a -> TTerm a
ifElse = primitive3 _logic_ifElse

not :: TTerm Bool -> TTerm Bool
not = primitive1 _logic_not

or :: TTerm Bool -> TTerm Bool -> TTerm Bool
or = primitive2 _logic_or
