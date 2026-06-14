-- | Phantom-typed term DSL for the hydra.lib.lists library

{-# LANGUAGE FlexibleContexts #-}

module Hydra.Dsl.Meta.Lib.Lists where

import Hydra.Typed
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Prims as Prims
import qualified Hydra.Lib.Lists as DefLists


-- | Apply a list of functions to a list of values (applicative style).
apply :: TypedTerm [a -> b] -> TypedTerm [a] -> TypedTerm [b]
apply = primitive2 (Prims.primName DefLists.apply)

-- | Apply a function that returns lists to each element and flatten results.
bind :: TypedTerm [a] -> TypedTerm (a -> [b]) -> TypedTerm [b]
bind = primitive2 (Prims.primName DefLists.bind)

-- | Concatenate a list of lists.
concat :: TypedTerm [[a]] -> TypedTerm [a]
concat = primitive1 (Prims.primName DefLists.concat)

-- | Concatenate two lists.
concat2 :: TypedTerm [a] -> TypedTerm [a] -> TypedTerm [a]
concat2 = primitive2 (Prims.primName DefLists.concat2)

-- | Prepend a value to a list.
cons :: TypedTerm a -> TypedTerm [a] -> TypedTerm [a]
cons = primitive2 (Prims.primName DefLists.cons)

-- | Drop the first n elements from a list.
drop :: TypedTerm Int -> TypedTerm [a] -> TypedTerm [a]
drop = primitive2 (Prims.primName DefLists.drop)

-- | Drop elements from the beginning of a list while predicate is true.
dropWhile :: TypedTerm (a -> Bool) -> TypedTerm [a] -> TypedTerm [a]
dropWhile = primitive2 (Prims.primName DefLists.dropWhile)

-- | Check if an element is in a list.
elem :: Eq a => TypedTerm a -> TypedTerm [a] -> TypedTerm Bool
elem = primitive2 (Prims.primName DefLists.elem)

-- | Filter a list based on a predicate.
filter :: AsTerm t [a] => TypedTerm (a -> Bool) -> t -> TypedTerm [a]
filter p xs = primitive2 (Prims.primName DefLists.filter) p (asTerm xs)

-- | Find the first element matching a predicate.
find :: TypedTerm (a -> Bool) -> TypedTerm [a] -> TypedTerm (Maybe a)
find = primitive2 (Prims.primName DefLists.find)

-- | Fold a list from the left.
foldl :: AsTerm f (b -> a -> b) => f -> TypedTerm b -> TypedTerm [a] -> TypedTerm b
foldl f = primitive3 (Prims.primName DefLists.foldl) (asTerm f)

-- | Fold a list from the right.
foldr :: AsTerm f (a -> b -> b) => f -> TypedTerm b -> TypedTerm [a] -> TypedTerm b
foldr f = primitive3 (Prims.primName DefLists.foldr) (asTerm f)

-- | Group consecutive equal elements.
group :: Eq a => TypedTerm [a] -> TypedTerm [[a]]
group = primitive1 (Prims.primName DefLists.group)

-- | Intercalate a list of lists with a separator list between each.
intercalate :: TypedTerm [a] -> TypedTerm [[a]] -> TypedTerm [a]
intercalate = primitive2 (Prims.primName DefLists.intercalate)

-- | Intersperse a value between elements of a list.
intersperse :: TypedTerm a -> TypedTerm [a] -> TypedTerm [a]
intersperse = primitive2 (Prims.primName DefLists.intersperse)

-- | Get the length of a list.
length :: TypedTerm [a] -> TypedTerm Int
length = primitive1 (Prims.primName DefLists.length)

-- | Map a function over a list.
map :: (AsTerm f (a -> b), AsTerm t [a]) => f -> t -> TypedTerm [b]
map f l = primitive2 (Prims.primName DefLists.map) (asTerm f) (asTerm l)

-- | Get the element at a specified index in a list, returning Nothing if out of bounds.
maybeAt :: TypedTerm Int -> TypedTerm [a] -> TypedTerm (Maybe a)
maybeAt = primitive2 (Prims.primName DefLists.maybeAt)

-- | Get the first element of a list, returning Nothing if the list is empty.
maybeHead :: TypedTerm [a] -> TypedTerm (Maybe a)
maybeHead = primitive1 (Prims.primName DefLists.maybeHead)

-- | Return all elements except the last one, returning Nothing if the list is empty.
maybeInit :: TypedTerm [a] -> TypedTerm (Maybe [a])
maybeInit = primitive1 (Prims.primName DefLists.maybeInit)

-- | Get the last element of a list, returning Nothing if the list is empty.
maybeLast :: TypedTerm [a] -> TypedTerm (Maybe a)
maybeLast = primitive1 (Prims.primName DefLists.maybeLast)

-- | Get all elements of a list except the first, returning Nothing if the list is empty.
maybeTail :: TypedTerm [a] -> TypedTerm (Maybe [a])
maybeTail = primitive1 (Prims.primName DefLists.maybeTail)

-- | Remove duplicate elements from a list.
nub :: Eq a => TypedTerm [a] -> TypedTerm [a]
nub = primitive1 (Prims.primName DefLists.nub)

-- | Check if a list is empty.
null :: TypedTerm [a] -> TypedTerm Bool
null = primitive1 (Prims.primName DefLists.null)

-- | Partition a list into elements that satisfy a predicate and elements that do not.
partition :: TypedTerm (a -> Bool) -> TypedTerm [a] -> TypedTerm ([a], [a])
partition = primitive2 (Prims.primName DefLists.partition)

-- | Create a list with a single element.
pure :: TypedTerm a -> TypedTerm [a]
pure = primitive1 (Prims.primName DefLists.pure)

-- | Create a list with n copies of a value.
replicate :: AsTerm t a => TypedTerm Int -> t -> TypedTerm [a]
replicate n x = primitive2 (Prims.primName DefLists.replicate) n (asTerm x)

-- | Reverse a list.
reverse :: TypedTerm [a] -> TypedTerm [a]
reverse = primitive1 (Prims.primName DefLists.reverse)

-- | Create a single-element list.
singleton :: TypedTerm a -> TypedTerm [a]
singleton = primitive1 (Prims.primName DefLists.singleton)

-- | Sort a list.
sort :: TypedTerm [a] -> TypedTerm [a]
sort = primitive1 (Prims.primName DefLists.sort)

-- | Sort a list based on a key function.
sortOn :: TypedTerm (a -> b) -> TypedTerm [a] -> TypedTerm [a]
sortOn = primitive2 (Prims.primName DefLists.sortOn)

-- | Split a list at the first element where predicate fails.
span :: TypedTerm (a -> Bool) -> TypedTerm [a] -> TypedTerm ([a], [a])
span = primitive2 (Prims.primName DefLists.span)

-- | Take the first n elements from a list.
take :: TypedTerm Int -> TypedTerm [a] -> TypedTerm [a]
take = primitive2 (Prims.primName DefLists.take)

-- | Transpose a list of lists.
transpose :: TypedTerm [[a]] -> TypedTerm [[a]]
transpose = primitive1 (Prims.primName DefLists.transpose)

-- | Decompose a list into its head and tail, returning Nothing if the list is empty.
uncons :: TypedTerm [a] -> TypedTerm (Maybe (a, [a]))
uncons = primitive1 (Prims.primName DefLists.uncons)

-- | Zip two lists into pairs.
zip :: TypedTerm [a] -> TypedTerm [b] -> TypedTerm [(a, b)]
zip = primitive2 (Prims.primName DefLists.zip)

-- | Zip two lists with a combining function.
zipWith :: TypedTerm (a -> b -> c) -> TypedTerm [a] -> TypedTerm [b] -> TypedTerm [c]
zipWith = primitive3 (Prims.primName DefLists.zipWith)
