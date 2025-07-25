-- | Haskell implementations of hydra.lib.maps primitives

module Hydra.Lib.Maps where

import qualified Data.Map as M


alter :: Ord k => (Maybe v -> Maybe v) -> k -> M.Map k v -> M.Map k v
alter = M.alter

bimap :: (Ord k1, Ord k2) => (k1 -> k2) -> (v1 -> v2) -> M.Map k1 v1 -> M.Map k2 v2
bimap f g = M.fromList . fmap (\(k, v) -> (f k, g v)) . M.toList

elems :: M.Map k v -> [v]
elems = M.elems

empty :: M.Map k v
empty = M.empty

filter :: Ord k => (v -> Bool) -> M.Map k v -> M.Map k v
filter = M.filter

filterWithKey :: Ord k => (k -> v -> Bool) -> M.Map k v -> M.Map k v
filterWithKey = M.filterWithKey

findWithDefault :: Ord k => v -> k -> M.Map k v -> v
findWithDefault = M.findWithDefault

fromList :: Ord k => [(k, v)] -> M.Map k v
fromList = M.fromList

insert :: Ord k => k -> v -> M.Map k v -> M.Map k v
insert = M.insert

keys :: M.Map k v -> [k]
keys = M.keys

lookup :: Ord k => k -> M.Map k v -> Maybe v
lookup = M.lookup

map :: (v1 -> v2) -> M.Map k v1 -> M.Map k v2
map = fmap

mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> M.Map k1 v -> M.Map k2 v
mapKeys = M.mapKeys

member :: Ord k => k -> M.Map k v -> Bool
member = M.member

null :: M.Map k v -> Bool
null = M.null

remove :: Ord k => k -> M.Map k v -> M.Map k v
remove = M.delete

singleton :: k -> v -> M.Map k v
singleton = M.singleton

size :: M.Map k v -> Int
size = M.size

toList :: M.Map k v -> [(k, v)]
toList = M.toList

union :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
union = M.union
