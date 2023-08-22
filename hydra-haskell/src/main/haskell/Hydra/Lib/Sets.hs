-- | Haskell implementations of hydra/lib/sets primitives

module Hydra.Lib.Sets where

import qualified Data.Set as S


contains :: Ord x => x -> S.Set x -> Bool
contains = S.member

difference :: Ord x => S.Set x -> S.Set x -> S.Set x
difference = S.difference

empty :: S.Set x
empty = S.empty

fromList :: Ord x => [x] -> S.Set x
fromList = S.fromList

insert :: Ord x => x -> S.Set x -> S.Set x
insert = S.insert

intersection :: Ord x => S.Set x -> S.Set x -> S.Set x
intersection = S.intersection

isEmpty :: S.Set x -> Bool
isEmpty = S.null

-- Note: the presence of a 'map' function does not imply that sets are a functor in Hydra
map :: Ord y => (x -> y) -> S.Set x -> S.Set y
map f = S.fromList . fmap f . S.toList

remove :: Ord x => x -> S.Set x -> S.Set x 
remove = S.delete

singleton :: x -> S.Set x
singleton = S.singleton

size :: S.Set x -> Int
size = S.size

toList :: Ord x => S.Set x -> [x]
toList = S.toList

union :: Ord x => S.Set x -> S.Set x -> S.Set x
union = S.union
