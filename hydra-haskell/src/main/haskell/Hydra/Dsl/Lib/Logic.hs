module Hydra.Dsl.Lib.Logic where

import Hydra.Core
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


and :: Datum (Bool -> Bool -> Bool)
and = Datum $ Terms.primitive _logic_and

ifElse :: Datum (a -> a -> Bool -> a)
ifElse = Datum $ Terms.primitive _logic_ifElse

not :: Datum (Bool -> Bool)
not = Datum $ Terms.primitive _logic_not

or :: Datum (Bool -> Bool -> Bool)
or = Datum $ Terms.primitive _logic_or
