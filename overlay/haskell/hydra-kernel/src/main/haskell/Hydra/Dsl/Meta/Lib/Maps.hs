{-# LANGUAGE FlexibleContexts #-}

-- | Phantom-typed term DSL for the hydra.lib.maps library

module Hydra.Dsl.Meta.Lib.Maps where

import Hydra.Typed
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms

import Data.Map
import qualified Hydra.Dsl.Prims as Prims
import qualified Hydra.Lib.Maps as DefMaps


-- | Alter a value at a key using a function.
alter :: TypedTerm (Maybe v -> Maybe v) -> TypedTerm k -> TypedTerm (Map k v) -> TypedTerm (Map k v)
alter = primitive3 (Prims.primName DefMaps.alter)

-- | Map a function over the keys and values of a map.
bimap :: TypedTerm (k1 -> k2) -> TypedTerm (v1 -> v2) -> TypedTerm (Map k1 v1) -> TypedTerm (Map k2 v2)
bimap = primitive3 (Prims.primName DefMaps.bimap)

-- | Remove a key from a map.
delete :: TypedTerm k -> TypedTerm (Map k v) -> TypedTerm (Map k v)
delete = primitive2 (Prims.primName DefMaps.delete)

-- | Get the values of a map.
elems :: TypedTerm (Map k v) -> TypedTerm [v]
elems = primitive1 (Prims.primName DefMaps.elems)

-- | Create an empty map.
empty :: TypedTerm (Map k v)
empty = primitive (Prims.primName DefMaps.empty)

-- | Filter a map based on values.
filter :: TypedTerm (v -> Bool) -> TypedTerm (Map k v) -> TypedTerm (Map k v)
filter = primitive2 (Prims.primName DefMaps.filter)

-- | Filter a map based on key-value pairs.
filterWithKey :: TypedTerm (k -> v -> Bool) -> TypedTerm (Map k v) -> TypedTerm (Map k v)
filterWithKey = primitive2 (Prims.primName DefMaps.filterWithKey)

-- | Lookup a value with a default.
findWithDefault :: AsTerm t v => t -> TypedTerm k -> TypedTerm (Map k v) -> TypedTerm v
findWithDefault def = primitive3 (Prims.primName DefMaps.findWithDefault) (asTerm def)

-- | Create a map from a list of key-value pairs.
fromList :: TypedTerm [(k, v)] -> TypedTerm (Map k v)
fromList = primitive1 (Prims.primName DefMaps.fromList)

-- | Insert a key-value pair into a map.
insert :: TypedTerm k -> TypedTerm v -> TypedTerm (Map k v) -> TypedTerm (Map k v)
insert = primitive3 (Prims.primName DefMaps.insert)

-- | Get the keys of a map.
keys :: TypedTerm (Map k v) -> TypedTerm [k]
keys = primitive1 (Prims.primName DefMaps.keys)

-- | Lookup a value in a map.
lookup :: AsTerm t k => t -> TypedTerm (Map k v) -> TypedTerm (Maybe v)
lookup k = primitive2 (Prims.primName DefMaps.lookup) (asTerm k)

-- | Map a function over a map.
map :: AsTerm f (v1 -> v2) => f -> TypedTerm (Map k v1) -> TypedTerm (Map k v2)
map f = primitive2 (Prims.primName DefMaps.map) (asTerm f)

-- | Map a function over the keys of a map.
mapKeys :: TypedTerm (k1 -> k2) -> TypedTerm (Map k1 v) -> TypedTerm (Map k2 v)
mapKeys = primitive2 (Prims.primName DefMaps.mapKeys)

-- | Check if a key is present in a map.
member :: TypedTerm k -> TypedTerm (Map k v) -> TypedTerm Bool
member = primitive2 (Prims.primName DefMaps.member)

-- | Check if a map is empty.
null :: TypedTerm (Map k v) -> TypedTerm Bool
null = primitive1 (Prims.primName DefMaps.null)

-- | Create a map with a single key-value pair.
singleton :: TypedTerm k -> TypedTerm v -> TypedTerm (Map k v)
singleton = primitive2 (Prims.primName DefMaps.singleton)

-- | Get the size of a map.
size :: TypedTerm (Map k v) -> TypedTerm Int
size = primitive1 (Prims.primName DefMaps.size)

-- | Convert a map to a list of key-value pairs.
toList :: TypedTerm (Map k v) -> TypedTerm [(k, v)]
toList = primitive1 (Prims.primName DefMaps.toList)

-- | Union two maps, with the first taking precedence.
union :: TypedTerm (Map k v) -> TypedTerm (Map k v) -> TypedTerm (Map k v)
union = primitive2 (Prims.primName DefMaps.union)
