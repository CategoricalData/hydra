-- | Phantom-typed term DSL for the hydra.lib.pairs library

module Hydra.Dsl.Lib.Pairs where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

first :: TTerm (a, b) -> TTerm a
first = primitive1 _pairs_first

second :: TTerm (a, b) -> TTerm b
second = primitive1 _pairs_second
