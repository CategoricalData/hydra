-- | Haskell implementations of hydra.lib.sets primitives

module Hydra.Lib.Sets where

import qualified Data.Set as S


-- | Delete an element from a set.
delete :: Ord x => x -> S.Set x -> S.Set x
delete = S.delete

-- | Compute the difference of two sets.
difference :: Ord x => S.Set x -> S.Set x -> S.Set x
difference = S.difference

-- | Create an empty set.
empty :: S.Set x
empty = S.empty

-- | Create a set from a list.
fromList :: Ord x => [x] -> S.Set x
fromList = S.fromList

-- | Insert an element into a set.
insert :: Ord x => x -> S.Set x -> S.Set x
insert = S.insert

-- | Compute the intersection of two sets.
intersection :: Ord x => S.Set x -> S.Set x -> S.Set x
intersection = S.intersection

-- | Map a function over a set.
-- Note: the presence of a 'map' function does not imply that sets are a functor in Hydra
map :: Ord y => (x -> y) -> S.Set x -> S.Set y
map f = S.fromList . fmap f . S.toList

-- | Check if an element is in a set.
member :: Ord x => x -> S.Set x -> Bool
member = S.member

-- | Check if a set is empty.
null :: S.Set x -> Bool
null = S.null

-- | Create a singleton set.
-- Consider renaming this to Sets.pure, or creating an alias
singleton :: x -> S.Set x
singleton = S.singleton

-- | Get the size of a set.
size :: S.Set x -> Int
size = S.size

-- | Convert a set to a list.
toList :: Ord x => S.Set x -> [x]
toList = S.toList

-- | Compute the union of two sets.
union :: Ord x => S.Set x -> S.Set x -> S.Set x
union = S.union

-- | Compute the union of multiple sets.
unions :: Ord x => [S.Set x] -> S.Set x
unions = S.unions
