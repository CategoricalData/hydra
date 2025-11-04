-- | Phantom-typed term DSL for the hydra.lib.tuples library

module Hydra.Dsl.Lib.Tuples where

import Hydra.Phantoms
import Hydra.Dsl.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


curry :: TTerm (a, b) -> TTerm (a -> b)
curry = primitive1 _tuples_curry

fst :: TTerm (a, b) -> TTerm a
fst = primitive1 _tuples_fst

snd :: TTerm (a, b) -> TTerm b
snd = primitive1 _tuples_snd

uncurry :: TTerm (a -> b) -> TTerm (a, b)
uncurry = primitive1 _tuples_uncurry
