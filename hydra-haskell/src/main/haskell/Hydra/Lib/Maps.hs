-- | Haskell implementations of hydra/lib/maps primitives

module Hydra.Lib.Maps where

import qualified Data.Map as M


empty :: M.Map k v
empty = M.empty

fromList :: Ord k => [(k, v)] -> M.Map k v
fromList = M.fromList

insert :: Ord k => k -> v -> M.Map k v -> M.Map k v
insert = M.insert

isEmpty :: M.Map k v -> Bool
isEmpty = M.null

keys :: M.Map k v -> [k]
keys = M.keys

lookup :: Ord k => k -> M.Map k v -> Maybe v
lookup = M.lookup

map :: (v1 -> v2) -> M.Map k v1 -> M.Map k v2
map = fmap

mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> M.Map k1 v -> M.Map k2 v
mapKeys = M.mapKeys

remove :: Ord k => k -> M.Map k v -> M.Map k v
remove = M.delete

singleton :: k -> v -> M.Map k v
singleton = M.singleton

size :: M.Map k v -> Int
size = M.size

toList :: M.Map k v -> [(k, v)]
toList = M.toList

values :: M.Map k v -> [v]
values = M.elems
