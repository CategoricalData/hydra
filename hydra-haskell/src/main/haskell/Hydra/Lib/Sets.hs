-- | Haskell implementations of hydra.lib.sets primitives

module Hydra.Lib.Sets where

import qualified Data.Set as S


delete :: Ord x => x -> S.Set x -> S.Set x
delete = S.delete

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

-- Note: the presence of a 'map' function does not imply that sets are a functor in Hydra
map :: Ord y => (x -> y) -> S.Set x -> S.Set y
map f = S.fromList . fmap f . S.toList

member :: Ord x => x -> S.Set x -> Bool
member = S.member

null :: S.Set x -> Bool
null = S.null

singleton :: x -> S.Set x
singleton = S.singleton

size :: S.Set x -> Int
size = S.size

toList :: Ord x => S.Set x -> [x]
toList = S.toList

union :: Ord x => S.Set x -> S.Set x -> S.Set x
union = S.union

unions :: Ord x => [S.Set x] -> S.Set x
unions = S.unions
