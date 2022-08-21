module Hydra.Lib.Sets where

import qualified Data.Set as S


add :: Ord a => a -> S.Set a -> S.Set a
add = S.insert

contains :: Ord a => a -> S.Set a -> Bool
contains = S.member

fromList :: Ord a => [a] -> S.Set a
fromList = S.fromList

isEmpty :: S.Set a -> Bool
isEmpty = S.null

-- Note: the presence of a 'map' function does not imply that sets are a functor in Hydra
map :: Ord b => (a -> b) -> S.Set a -> S.Set b
map f = S.fromList . fmap f . S.toList

remove :: Ord a => a -> S.Set a -> S.Set a
remove = S.delete

singleton :: a -> S.Set a
singleton = S.singleton

toList :: Ord a => S.Set a -> [a]
toList = S.toList
