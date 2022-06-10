module Hydra.Impl.Haskell.Dsl.Lib.Sets where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries

import Data.Set


--add :: Trm (a -> Set a -> Set a)
--add = Trm $ Terms.primitive _sets_add

contains :: Trm (a -> Set a -> Bool)
contains = Trm $ Terms.primitive _sets_contains

isEmpty :: Trm (Set a -> Bool)
isEmpty = Trm $ Terms.primitive _sets_isEmpty

remove :: Trm (a -> Set a -> Set a)
remove = Trm $ Terms.primitive _sets_remove
