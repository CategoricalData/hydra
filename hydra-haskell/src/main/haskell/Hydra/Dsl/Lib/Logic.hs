module Hydra.Dsl.Lib.Logic where

import Hydra.Core
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


and :: TTerm (Bool -> Bool -> Bool)
and = TTerm $ Terms.primitive _logic_and

ifElse :: TTerm (a -> a -> Bool -> a)
ifElse = TTerm $ Terms.primitive _logic_ifElse

not :: TTerm (Bool -> Bool)
not = TTerm $ Terms.primitive _logic_not

or :: TTerm (Bool -> Bool -> Bool)
or = TTerm $ Terms.primitive _logic_or
