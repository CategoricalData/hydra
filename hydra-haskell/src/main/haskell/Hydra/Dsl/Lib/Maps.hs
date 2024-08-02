module Hydra.Dsl.Lib.Maps where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms

import Data.Map


empty :: TTerm (Map k v)
empty = TTerm $ Terms.primitive _maps_empty

fromList :: TTerm ([(k, v)] -> Map k v)
fromList = TTerm $ Terms.primitive _maps_fromList

insert :: TTerm (k -> v -> Map k v -> Map k v)
insert = TTerm $ Terms.primitive _maps_insert

isEmpty :: TTerm (Map k v -> Bool)
isEmpty = TTerm $ Terms.primitive _maps_isEmpty

keys :: TTerm (Map k v -> [k])
keys = TTerm $ Terms.primitive _maps_keys

lookup :: TTerm (k -> Map k v -> Maybe v)
lookup = TTerm $ Terms.primitive _maps_lookup

map :: TTerm ((v1 -> v2) -> Map k v1 -> Map k v2)
map = TTerm $ Terms.primitive _maps_map

mapKeys :: TTerm ((k1 -> k2) -> Map k1 v -> Map k2 v)
mapKeys = TTerm $ Terms.primitive _maps_mapKeys

remove :: TTerm (k -> Map k v -> Map k v)
remove = TTerm $ Terms.primitive _maps_remove

singleton :: TTerm (k -> v -> Map k v)
singleton = TTerm $ Terms.primitive _maps_singleton

size :: TTerm (Map k v -> Int)
size = TTerm $ Terms.primitive _maps_size

toList :: TTerm (Map k v -> [(k, v)])
toList = TTerm $ Terms.primitive _maps_toList

values :: TTerm (Map k v -> [v])
values = TTerm $ Terms.primitive _maps_values
