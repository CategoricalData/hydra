-- | Phantom-typed term DSL for the hydra.lib.pairs library

module Hydra.Dsl.Meta.Lib.Pairs where

import Hydra.Typed
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Map over both elements of a pair.
bimap :: TypedTerm (a -> c) -> TypedTerm (b -> d) -> TypedTerm (a, b) -> TypedTerm (c, d)
bimap = primitive3 _pairs_bimap

-- | Get the first element of a pair.
first :: TypedTerm (a, b) -> TypedTerm a
first = primitive1 _pairs_first

-- | Get the second element of a pair.
second :: TypedTerm (a, b) -> TypedTerm b
second = primitive1 _pairs_second
