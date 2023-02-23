-- | Haskell implementations of hydra/lib/sets primitives

module Hydra.Lib.Sets where

import qualified Data.Set as S


contains :: Ord a => a -> S.Set a -> Bool
contains = S.member

empty :: S.Set a
empty = S.empty

fromList :: Ord a => [a] -> S.Set a
fromList = S.fromList

insert :: Ord a => a -> S.Set a -> S.Set a
insert = S.insert

isEmpty :: S.Set a -> Bool
isEmpty = S.null

-- Note: the presence of a 'map' function does not imply that sets are a functor in Hydra
map :: Ord b => (a -> b) -> S.Set a -> S.Set b
map f = S.fromList . fmap f . S.toList

remove :: Ord a => a -> S.Set a -> S.Set a
remove = S.delete

singleton :: a -> S.Set a
singleton = S.singleton

size :: S.Set a -> Int
size = S.size

toList :: Ord a => S.Set a -> [a]
toList = S.toList
