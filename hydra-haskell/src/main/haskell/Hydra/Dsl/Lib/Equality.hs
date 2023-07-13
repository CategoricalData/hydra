module Hydra.Dsl.Lib.Equality where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


equalTerm :: Datum (Term a -> Term a -> Bool)
equalTerm = Datum $ Terms.primitive _equality_equalTerm
