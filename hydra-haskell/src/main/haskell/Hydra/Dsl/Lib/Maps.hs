module Hydra.Dsl.Lib.Maps where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms

import Data.Map


bimap :: TTerm (k1 -> k2) -> TTerm (v1 -> v2) -> TTerm (Map k1 v1) -> TTerm (Map k2 v2)
bimap = primitive3 _maps_bimap

empty :: TTerm (Map k v)
empty = primitive _maps_empty

filter :: TTerm (v -> Bool) -> TTerm (Map k v) -> TTerm (Map k v)
filter = primitive2 _maps_filter

filterWithKey :: TTerm (k -> v -> Bool) -> TTerm (Map k v) -> TTerm (Map k v)
filterWithKey = primitive2 _maps_filterWithKey

fromList :: TTerm [(k, v)] -> TTerm (Map k v)
fromList = primitive1 _maps_fromList

insert :: TTerm k -> TTerm v -> TTerm (Map k v) -> TTerm (Map k v)
insert = primitive3 _maps_insert

isEmpty :: TTerm (Map k v) -> TTerm Bool
isEmpty = primitive1 _maps_isEmpty

keys :: TTerm (Map k v) -> TTerm [k]
keys = primitive1 _maps_keys

lookup :: TTerm k -> TTerm (Map k v) -> TTerm (Maybe v)
lookup = primitive2 _maps_lookup

map :: TTerm (v1 -> v2) -> TTerm (Map k v1) -> TTerm (Map k v2)
map = primitive2 _maps_map

mapKeys :: TTerm (k1 -> k2) -> TTerm (Map k1 v) -> TTerm (Map k2 v)
mapKeys = primitive2 _maps_mapKeys

remove :: TTerm k -> TTerm (Map k v) -> TTerm (Map k v)
remove = primitive2 _maps_remove

singleton :: TTerm k -> TTerm v -> TTerm (Map k v)
singleton = primitive2 _maps_singleton

size :: TTerm (Map k v) -> TTerm Int
size = primitive1 _maps_size

toList :: TTerm (Map k v) -> TTerm [(k, v)]
toList = primitive1 _maps_toList

union :: TTerm (Map k v) -> TTerm (Map k v) -> TTerm (Map k v)
union = primitive2 _maps_union

values :: TTerm (Map k v) -> TTerm [v]
values = primitive1 _maps_values
