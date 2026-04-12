{-# LANGUAGE FlexibleContexts #-}

-- | Phantom-typed term DSL for the hydra.lib.maps library

module Hydra.Dsl.Meta.Lib.Maps where

import Hydra.Phantoms
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

import Data.Map


-- | Alter a value at a key using a function.
alter :: TTerm (Maybe v -> Maybe v) -> TTerm k -> TTerm (Map k v) -> TTerm (Map k v)
alter = primitive3 _maps_alter

-- | Map a function over the keys and values of a map.
bimap :: TTerm (k1 -> k2) -> TTerm (v1 -> v2) -> TTerm (Map k1 v1) -> TTerm (Map k2 v2)
bimap = primitive3 _maps_bimap

-- | Remove a key from a map.
delete :: TTerm k -> TTerm (Map k v) -> TTerm (Map k v)
delete = primitive2 _maps_delete

-- | Get the values of a map.
elems :: TTerm (Map k v) -> TTerm [v]
elems = primitive1 _maps_elems

-- | Create an empty map.
empty :: TTerm (Map k v)
empty = primitive _maps_empty

-- | Filter a map based on values.
filter :: TTerm (v -> Bool) -> TTerm (Map k v) -> TTerm (Map k v)
filter = primitive2 _maps_filter

-- | Filter a map based on key-value pairs.
filterWithKey :: TTerm (k -> v -> Bool) -> TTerm (Map k v) -> TTerm (Map k v)
filterWithKey = primitive2 _maps_filterWithKey

-- | Lookup a value with a default.
findWithDefault :: AsTerm t v => t -> TTerm k -> TTerm (Map k v) -> TTerm v
findWithDefault def = primitive3 _maps_findWithDefault (asTerm def)

-- | Create a map from a list of key-value pairs.
fromList :: TTerm [(k, v)] -> TTerm (Map k v)
fromList = primitive1 _maps_fromList

-- | Insert a key-value pair into a map.
insert :: TTerm k -> TTerm v -> TTerm (Map k v) -> TTerm (Map k v)
insert = primitive3 _maps_insert

-- | Get the keys of a map.
keys :: TTerm (Map k v) -> TTerm [k]
keys = primitive1 _maps_keys

-- | Lookup a value in a map.
lookup :: AsTerm t k => t -> TTerm (Map k v) -> TTerm (Maybe v)
lookup k = primitive2 _maps_lookup (asTerm k)

-- | Map a function over a map.
map :: AsTerm f (v1 -> v2) => f -> TTerm (Map k v1) -> TTerm (Map k v2)
map f = primitive2 _maps_map (asTerm f)

-- | Map a function over the keys of a map.
mapKeys :: TTerm (k1 -> k2) -> TTerm (Map k1 v) -> TTerm (Map k2 v)
mapKeys = primitive2 _maps_mapKeys

-- | Check if a key is present in a map.
member :: TTerm k -> TTerm (Map k v) -> TTerm Bool
member = primitive2 _maps_member

-- | Check if a map is empty.
null :: TTerm (Map k v) -> TTerm Bool
null = primitive1 _maps_null

-- | Create a map with a single key-value pair.
singleton :: TTerm k -> TTerm v -> TTerm (Map k v)
singleton = primitive2 _maps_singleton

-- | Get the size of a map.
size :: TTerm (Map k v) -> TTerm Int
size = primitive1 _maps_size

-- | Convert a map to a list of key-value pairs.
toList :: TTerm (Map k v) -> TTerm [(k, v)]
toList = primitive1 _maps_toList

-- | Union two maps, with the first taking precedence.
union :: TTerm (Map k v) -> TTerm (Map k v) -> TTerm (Map k v)
union = primitive2 _maps_union
