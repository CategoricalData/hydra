-- | Phantom-typed term DSL for the hydra.lib.flows library

module Hydra.Dsl.Lib.Flows where

import Hydra.Dsl.Phantoms
import Hydra.Core
import Hydra.Compute
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Set as S


apply :: TTerm (Flow s (x -> y)) -> TTerm (Flow s x) -> TTerm (Flow s y)
apply = primitive2 _flows_apply

bind :: TTerm (Flow s x) -> TTerm (x -> Flow s y) -> TTerm (Flow s y)
bind = primitive2 _flows_bind

fail :: TTerm String -> TTerm (Flow s x)
fail = primitive1 _flows_fail

foldl :: TTerm (x -> y -> Flow s x) -> TTerm x -> TTerm [y] -> TTerm (Flow s x)
foldl = primitive3 _flows_foldl

map :: TTerm (x -> y) -> TTerm (Flow s x) -> TTerm (Flow s y)
map = primitive2 _flows_map

mapElems :: TTerm (v1 -> Flow s v2) -> TTerm (M.Map k v1) -> TTerm (Flow s (M.Map k v2))
mapElems = primitive2 _flows_mapElems

mapKeys :: TTerm (k1 -> Flow s k2) -> TTerm (M.Map k1 v) -> TTerm (Flow s (M.Map k2 v))
mapKeys = primitive2 _flows_mapKeys

mapList :: TTerm (x -> Flow s y) -> TTerm [x] -> TTerm (Flow s [y])
mapList = primitive2 _flows_mapList

mapOptional :: TTerm (x -> Flow s y) -> TTerm (Maybe x) -> TTerm (Flow s (Maybe y))
mapOptional = primitive2 _flows_mapOptional

mapSet :: TTerm (x -> Flow s y) -> TTerm (S.Set x) -> TTerm (Flow s (S.Set y))
mapSet = primitive2 _flows_mapSet

pure :: TTerm x -> TTerm (Flow s x)
pure = primitive1 _flows_pure

sequence :: TTerm [Flow s a] -> TTerm (Flow s [a])
sequence = primitive1 _flows_sequence
