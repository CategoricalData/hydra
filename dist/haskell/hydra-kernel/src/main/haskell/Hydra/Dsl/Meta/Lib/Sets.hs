{-# LANGUAGE FlexibleContexts #-}

-- | Phantom-typed term DSL for the hydra.lib.sets library

module Hydra.Dsl.Meta.Lib.Sets where

import Hydra.Typed
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms

import Data.Set
import qualified Hydra.Dsl.Prims as Prims
import qualified Hydra.Lib.Sets as DefSets


-- | Delete an element from a set.
delete :: AsTerm t a => t -> TypedTerm (Set a) -> TypedTerm (Set a)
delete x = primitive2 DefSets.delete (asTerm x)

-- | Compute the difference of two sets.
difference :: TypedTerm (Set a) -> TypedTerm (Set a) -> TypedTerm (Set a)
difference = primitive2 DefSets.difference

-- | Create an empty set.
empty :: TypedTerm (Set a)
empty = primitive DefSets.empty

-- | Create a set from a list.
fromList :: AsTerm t [a] => t -> TypedTerm (Set a)
fromList xs = primitive1 DefSets.fromList (asTerm xs)

-- | Insert an element into a set.
insert :: TypedTerm a -> TypedTerm (Set a) -> TypedTerm (Set a)
insert = primitive2 DefSets.insert

-- | Compute the intersection of two sets.
intersection :: TypedTerm (Set a) -> TypedTerm (Set a) -> TypedTerm (Set a)
intersection = primitive2 DefSets.intersection

-- | Map a function over a set.
map :: TypedTerm (a -> b) -> TypedTerm (Set a) -> TypedTerm (Set b)
map = primitive2 DefSets.map

-- | Check if an element is in a set.
member :: TypedTerm a -> TypedTerm (Set a) -> TypedTerm Bool
member = primitive2 DefSets.member

-- | Check if a set is empty.
null :: TypedTerm (Set a) -> TypedTerm Bool
null = primitive1 DefSets.null

-- | Create a singleton set.
singleton :: TypedTerm a -> TypedTerm (Set a)
singleton = primitive1 DefSets.singleton

-- | Get the size of a set.
size :: TypedTerm (Set a) -> TypedTerm Int
size = primitive1 DefSets.size

-- | Convert a set to a list.
toList :: TypedTerm (Set a) -> TypedTerm [a]
toList = primitive1 DefSets.toList

-- | Compute the union of two sets.
union :: TypedTerm (Set a) -> TypedTerm (Set a) -> TypedTerm (Set a)
union = primitive2 DefSets.union

-- | Compute the union of multiple sets.
unions :: TypedTerm [Set a] -> TypedTerm (Set a)
unions = primitive1 DefSets.unions
