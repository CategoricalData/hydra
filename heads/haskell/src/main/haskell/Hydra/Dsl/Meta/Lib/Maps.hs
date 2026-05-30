{-# LANGUAGE FlexibleContexts #-}

-- | Phantom-typed term DSL for the hydra.lib.maps library

module Hydra.Dsl.Meta.Lib.Maps where

import Hydra.Typed
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

import Data.Map


-- | Alter a value at a key using a function.
alter :: TypedTerm (Maybe v -> Maybe v) -> TypedTerm k -> TypedTerm (Map k v) -> TypedTerm (Map k v)
alter = primitive3 _maps_alter

-- | Map a function over the keys and values of a map.
bimap :: TypedTerm (k1 -> k2) -> TypedTerm (v1 -> v2) -> TypedTerm (Map k1 v1) -> TypedTerm (Map k2 v2)
bimap = primitive3 _maps_bimap

-- | Remove a key from a map.
delete :: TypedTerm k -> TypedTerm (Map k v) -> TypedTerm (Map k v)
delete = primitive2 _maps_delete

-- | Get the values of a map.
elems :: TypedTerm (Map k v) -> TypedTerm [v]
elems = primitive1 _maps_elems

-- | Create an empty map.
empty :: TypedTerm (Map k v)
empty = primitive _maps_empty

-- | Filter a map based on values.
filter :: TypedTerm (v -> Bool) -> TypedTerm (Map k v) -> TypedTerm (Map k v)
filter = primitive2 _maps_filter

-- | Filter a map based on key-value pairs.
filterWithKey :: TypedTerm (k -> v -> Bool) -> TypedTerm (Map k v) -> TypedTerm (Map k v)
filterWithKey = primitive2 _maps_filterWithKey

-- | Lookup a value with a default.
findWithDefault :: AsTerm t v => t -> TypedTerm k -> TypedTerm (Map k v) -> TypedTerm v
findWithDefault def = primitive3 _maps_findWithDefault (asTerm def)

-- | Create a map from a list of key-value pairs.
fromList :: TypedTerm [(k, v)] -> TypedTerm (Map k v)
fromList = primitive1 _maps_fromList

-- | Insert a key-value pair into a map.
insert :: TypedTerm k -> TypedTerm v -> TypedTerm (Map k v) -> TypedTerm (Map k v)
insert = primitive3 _maps_insert

-- | Get the keys of a map.
keys :: TypedTerm (Map k v) -> TypedTerm [k]
keys = primitive1 _maps_keys

-- | Lookup a value in a map.
lookup :: AsTerm t k => t -> TypedTerm (Map k v) -> TypedTerm (Maybe v)
lookup k = primitive2 _maps_lookup (asTerm k)

-- | Map a function over a map.
map :: AsTerm f (v1 -> v2) => f -> TypedTerm (Map k v1) -> TypedTerm (Map k v2)
map f = primitive2 _maps_map (asTerm f)

-- | Map a function over the keys of a map.
mapKeys :: TypedTerm (k1 -> k2) -> TypedTerm (Map k1 v) -> TypedTerm (Map k2 v)
mapKeys = primitive2 _maps_mapKeys

-- | Check if a key is present in a map.
member :: TypedTerm k -> TypedTerm (Map k v) -> TypedTerm Bool
member = primitive2 _maps_member

-- | Check if a map is empty.
null :: TypedTerm (Map k v) -> TypedTerm Bool
null = primitive1 _maps_null

-- | Create a map with a single key-value pair.
singleton :: TypedTerm k -> TypedTerm v -> TypedTerm (Map k v)
singleton = primitive2 _maps_singleton

-- | Get the size of a map.
size :: TypedTerm (Map k v) -> TypedTerm Int
size = primitive1 _maps_size

-- | Convert a map to a list of key-value pairs.
toList :: TypedTerm (Map k v) -> TypedTerm [(k, v)]
toList = primitive1 _maps_toList

-- | Union two maps, with the first taking precedence.
union :: TypedTerm (Map k v) -> TypedTerm (Map k v) -> TypedTerm (Map k v)
union = primitive2 _maps_union
