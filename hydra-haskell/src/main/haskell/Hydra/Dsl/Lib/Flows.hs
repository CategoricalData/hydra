module Hydra.Dsl.Lib.Flows where

import Hydra.Dsl.Phantoms
import Hydra.Core
import Hydra.Compute
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M


-- Primitives

apply :: TTerm (Flow s (x -> y)) -> TTerm (Flow s x) -> TTerm (Flow s y)
apply = primitive2 _flows_apply

bind :: TTerm (Flow s x) -> TTerm (x -> Flow s y) -> TTerm (Flow s y)
bind = primitive2 _flows_bind

fail :: TTerm String -> TTerm (Flow s x)
fail = primitive1 _flows_fail

map :: TTerm (x -> y) -> TTerm (Flow s x) -> TTerm (Flow s y)
map = primitive2 _flows_map

mapList :: TTerm (x -> Flow s y) -> TTerm [x] -> TTerm (Flow s [y])
mapList = primitive2 _flows_mapList

pure :: TTerm x -> TTerm (Flow s x)
pure = primitive1 _flows_pure

sequence :: TTerm [Flow s a] -> TTerm (Flow s [a])
sequence = primitive1 _flows_sequence
