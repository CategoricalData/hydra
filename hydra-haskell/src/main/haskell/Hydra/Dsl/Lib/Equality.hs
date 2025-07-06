module Hydra.Dsl.Lib.Equality where

import Hydra.Core
import Hydra.Graph
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms

import Data.Int


compare :: TTerm a -> TTerm a -> TTerm Comparison
compare = primitive2 _equality_compare

equal :: TTerm a -> TTerm a -> TTerm Bool
equal = primitive2 _equality_equal

gt :: TTerm a -> TTerm a -> TTerm Bool
gt = primitive2 _equality_gt

gte :: TTerm a -> TTerm a -> TTerm Bool
gte = primitive2 _equality_gte

identity :: TTerm a -> TTerm a
identity = primitive1 _equality_identity

lt :: TTerm a -> TTerm a -> TTerm Bool
lt = primitive2 _equality_lt

lte :: TTerm a -> TTerm a -> TTerm Bool
lte = primitive2 _equality_lte
