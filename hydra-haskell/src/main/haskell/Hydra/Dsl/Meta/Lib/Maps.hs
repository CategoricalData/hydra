-- | Phantom-typed term DSL for the hydra.lib.maps library

module Hydra.Dsl.Meta.Lib.Maps where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

import Data.Map


alter :: TTerm (Maybe v -> Maybe v) -> TTerm k -> TTerm (Map k v) -> TTerm (Map k v)
alter = primitive3 _maps_alter

bimap :: TTerm (k1 -> k2) -> TTerm (v1 -> v2) -> TTerm (Map k1 v1) -> TTerm (Map k2 v2)
bimap = primitive3 _maps_bimap

elems :: TTerm (Map k v) -> TTerm [v]
elems = primitive1 _maps_elems

empty :: TTerm (Map k v)
empty = primitive _maps_empty

filter :: TTerm (v -> Bool) -> TTerm (Map k v) -> TTerm (Map k v)
filter = primitive2 _maps_filter

filterWithKey :: TTerm (k -> v -> Bool) -> TTerm (Map k v) -> TTerm (Map k v)
filterWithKey = primitive2 _maps_filterWithKey

findWithDefault :: TTerm v -> TTerm k -> TTerm (Map k v) -> TTerm v
findWithDefault = primitive3 _maps_findWithDefault

fromList :: TTerm [(k, v)] -> TTerm (Map k v)
fromList = primitive1 _maps_fromList

insert :: TTerm k -> TTerm v -> TTerm (Map k v) -> TTerm (Map k v)
insert = primitive3 _maps_insert

keys :: TTerm (Map k v) -> TTerm [k]
keys = primitive1 _maps_keys

lookup :: TTerm k -> TTerm (Map k v) -> TTerm (Maybe v)
lookup = primitive2 _maps_lookup

map :: TTerm (v1 -> v2) -> TTerm (Map k v1) -> TTerm (Map k v2)
map = primitive2 _maps_map

mapKeys :: TTerm (k1 -> k2) -> TTerm (Map k1 v) -> TTerm (Map k2 v)
mapKeys = primitive2 _maps_mapKeys

member :: TTerm k -> TTerm (Map k v) -> TTerm Bool
member = primitive2 _maps_member

null :: TTerm (Map k v) -> TTerm Bool
null = primitive1 _maps_null

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
