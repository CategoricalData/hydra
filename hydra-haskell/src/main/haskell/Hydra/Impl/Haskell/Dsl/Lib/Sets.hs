module Hydra.Impl.Haskell.Dsl.Lib.Sets where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries

import Data.Set


--add :: Data (a -> Set a -> Set a)
--add = Data $ Terms.primitive _sets_add

contains :: Data (a -> Set a -> Bool)
contains = Data $ Terms.primitive _sets_contains

isEmpty :: Data (Set a -> Bool)
isEmpty = Data $ Terms.primitive _sets_isEmpty

remove :: Data (a -> Set a -> Set a)
remove = Data $ Terms.primitive _sets_remove
