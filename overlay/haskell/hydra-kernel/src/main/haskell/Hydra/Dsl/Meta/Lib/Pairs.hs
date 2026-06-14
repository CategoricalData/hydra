-- | Phantom-typed term DSL for the hydra.lib.pairs library

module Hydra.Dsl.Meta.Lib.Pairs where

import Hydra.Typed
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Prims as Prims
import qualified Hydra.Lib.Pairs as DefPairs


-- | Map over both elements of a pair.
bimap :: TypedTerm (a -> c) -> TypedTerm (b -> d) -> TypedTerm (a, b) -> TypedTerm (c, d)
bimap = primitive3 (Prims.primName DefPairs.bimap)

-- | Get the first element of a pair.
first :: TypedTerm (a, b) -> TypedTerm a
first = primitive1 (Prims.primName DefPairs.first)

-- | Get the second element of a pair.
second :: TypedTerm (a, b) -> TypedTerm b
second = primitive1 (Prims.primName DefPairs.second)
