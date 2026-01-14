-- | Haskell implementations of hydra.lib.maps primitives

module Hydra.Lib.Maps where

import qualified Data.Map as M


-- | Alter a value at a key using a function. The function receives Nothing if the key is absent,
-- or Just the current value if present. Return Nothing to delete, Just to insert/update.
alter :: Ord k => (Maybe v -> Maybe v) -> k -> M.Map k v -> M.Map k v
alter = M.alter

-- | Map a function over the keys and values of a map.
bimap :: (Ord k1, Ord k2) => (k1 -> k2) -> (v1 -> v2) -> M.Map k1 v1 -> M.Map k2 v2
bimap f g = M.fromList . fmap (\(k, v) -> (f k, g v)) . M.toList

-- | Remove a key from a map.
delete :: Ord k => k -> M.Map k v -> M.Map k v
delete = M.delete

-- | Get the values of a map.
elems :: M.Map k v -> [v]
elems = M.elems

-- | Create an empty map.
empty :: M.Map k v
empty = M.empty

-- | Filter a map based on values.
filter :: Ord k => (v -> Bool) -> M.Map k v -> M.Map k v
filter = M.filter

-- | Filter a map based on key-value pairs.
filterWithKey :: Ord k => (k -> v -> Bool) -> M.Map k v -> M.Map k v
filterWithKey = M.filterWithKey

-- | Lookup a value with a default.
findWithDefault :: Ord k => v -> k -> M.Map k v -> v
findWithDefault = M.findWithDefault

-- | Create a map from a list of key-value pairs.
fromList :: Ord k => [(k, v)] -> M.Map k v
fromList = M.fromList

-- | Insert a key-value pair into a map.
insert :: Ord k => k -> v -> M.Map k v -> M.Map k v
insert = M.insert

-- | Get the keys of a map.
keys :: M.Map k v -> [k]
keys = M.keys

-- | Lookup a value in a map.
lookup :: Ord k => k -> M.Map k v -> Maybe v
lookup = M.lookup

-- | Map a function over a map.
map :: (v1 -> v2) -> M.Map k v1 -> M.Map k v2
map = fmap

-- | Map a function over the keys of a map.
mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> M.Map k1 v -> M.Map k2 v
mapKeys = M.mapKeys

-- | Check if a key is present in a map.
member :: Ord k => k -> M.Map k v -> Bool
member = M.member

-- | Check if a map is empty.
null :: M.Map k v -> Bool
null = M.null

-- | Create a map with a single key-value pair.
singleton :: k -> v -> M.Map k v
singleton = M.singleton

-- | Get the size of a map.
size :: M.Map k v -> Int
size = M.size

-- | Convert a map to a list of key-value pairs.
toList :: M.Map k v -> [(k, v)]
toList = M.toList

-- | Union two maps, with the first taking precedence.
union :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
union = M.union
