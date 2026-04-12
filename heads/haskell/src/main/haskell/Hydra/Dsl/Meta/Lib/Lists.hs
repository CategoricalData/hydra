-- | Phantom-typed term DSL for the hydra.lib.lists library

{-# LANGUAGE FlexibleContexts #-}

module Hydra.Dsl.Meta.Lib.Lists where

import Hydra.Phantoms
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Apply a list of functions to a list of values (applicative style).
apply :: TTerm [a -> b] -> TTerm [a] -> TTerm [b]
apply = primitive2 _lists_apply

-- | Get the element at a specified index in a list.
at :: TTerm Int -> TTerm [a] -> TTerm a
at = primitive2 _lists_at

-- | Apply a function that returns lists to each element and flatten results.
bind :: TTerm [a] -> TTerm (a -> [b]) -> TTerm [b]
bind = primitive2 _lists_bind

-- | Concatenate a list of lists.
concat :: TTerm [[a]] -> TTerm [a]
concat = primitive1 _lists_concat

-- | Concatenate two lists.
concat2 :: TTerm [a] -> TTerm [a] -> TTerm [a]
concat2 = primitive2 _lists_concat2

-- | Prepend a value to a list.
cons :: TTerm a -> TTerm [a] -> TTerm [a]
cons = primitive2 _lists_cons

-- | Drop the first n elements from a list.
drop :: TTerm Int -> TTerm [a] -> TTerm [a]
drop = primitive2 _lists_drop

-- | Drop elements from the beginning of a list while predicate is true.
dropWhile :: TTerm (a -> Bool) -> TTerm [a] -> TTerm [a]
dropWhile = primitive2 _lists_dropWhile

-- | Check if an element is in a list.
elem :: Eq a => TTerm a -> TTerm [a] -> TTerm Bool
elem = primitive2 _lists_elem

-- | Filter a list based on a predicate.
filter :: AsTerm t [a] => TTerm (a -> Bool) -> t -> TTerm [a]
filter p xs = primitive2 _lists_filter p (asTerm xs)

-- | Find the first element matching a predicate.
find :: TTerm (a -> Bool) -> TTerm [a] -> TTerm (Maybe a)
find = primitive2 _lists_find

-- | Fold a list from the left.
foldl :: AsTerm f (b -> a -> b) => f -> TTerm b -> TTerm [a] -> TTerm b
foldl f = primitive3 _lists_foldl (asTerm f)

-- | Fold a list from the right.
foldr :: AsTerm f (a -> b -> b) => f -> TTerm b -> TTerm [a] -> TTerm b
foldr f = primitive3 _lists_foldr (asTerm f)

-- | Group consecutive equal elements.
group :: Eq a => TTerm [a] -> TTerm [[a]]
group = primitive1 _lists_group

-- | Get the first element of a list.
head :: TTerm [a] -> TTerm a
head = primitive1 _lists_head

-- | Return all elements except the last one.
init :: TTerm [a] -> TTerm [a]
init = primitive1 _lists_init

-- | Intercalate a list of lists with a separator list between each.
intercalate :: TTerm [a] -> TTerm [[a]] -> TTerm [a]
intercalate = primitive2 _lists_intercalate

-- | Intersperse a value between elements of a list.
intersperse :: TTerm a -> TTerm [a] -> TTerm [a]
intersperse = primitive2 _lists_intersperse

-- | Get the last element of a list.
last :: TTerm [a] -> TTerm a
last = primitive1 _lists_last

-- | Get the length of a list.
length :: TTerm [a] -> TTerm Int
length = primitive1 _lists_length

-- | Map a function over a list.
map :: (AsTerm f (a -> b), AsTerm t [a]) => f -> t -> TTerm [b]
map f l = primitive2 _lists_map (asTerm f) (asTerm l)

-- | Get the element at a specified index in a list, returning Nothing if out of bounds.
maybeAt :: TTerm Int -> TTerm [a] -> TTerm (Maybe a)
maybeAt = primitive2 _lists_maybeAt

-- | Get the first element of a list, returning Nothing if the list is empty.
maybeHead :: TTerm [a] -> TTerm (Maybe a)
maybeHead = primitive1 _lists_maybeHead

-- | Return all elements except the last one, returning Nothing if the list is empty.
maybeInit :: TTerm [a] -> TTerm (Maybe [a])
maybeInit = primitive1 _lists_maybeInit

-- | Get the last element of a list, returning Nothing if the list is empty.
maybeLast :: TTerm [a] -> TTerm (Maybe a)
maybeLast = primitive1 _lists_maybeLast

-- | Get all elements of a list except the first, returning Nothing if the list is empty.
maybeTail :: TTerm [a] -> TTerm (Maybe [a])
maybeTail = primitive1 _lists_maybeTail

-- | Remove duplicate elements from a list.
nub :: Eq a => TTerm [a] -> TTerm [a]
nub = primitive1 _lists_nub

-- | Check if a list is empty.
null :: TTerm [a] -> TTerm Bool
null = primitive1 _lists_null

-- | Partition a list into elements that satisfy a predicate and elements that do not.
partition :: TTerm (a -> Bool) -> TTerm [a] -> TTerm ([a], [a])
partition = primitive2 _lists_partition

-- | Create a list with a single element.
pure :: TTerm a -> TTerm [a]
pure = primitive1 _lists_pure

-- | Create a list with n copies of a value.
replicate :: AsTerm t a => TTerm Int -> t -> TTerm [a]
replicate n x = primitive2 _lists_replicate n (asTerm x)

-- | Reverse a list.
reverse :: TTerm [a] -> TTerm [a]
reverse = primitive1 _lists_reverse

-- | Get the first element of a list, returning Nothing if the list is empty.
-- Deprecated: use maybeHead instead.
safeHead :: TTerm [a] -> TTerm (Maybe a)
safeHead = primitive1 _lists_safeHead

-- | Create a single-element list.
singleton :: TTerm a -> TTerm [a]
singleton = primitive1 _lists_singleton

-- | Sort a list.
sort :: TTerm [a] -> TTerm [a]
sort = primitive1 _lists_sort

-- | Sort a list based on a key function.
sortOn :: TTerm (a -> b) -> TTerm [a] -> TTerm [a]
sortOn = primitive2 _lists_sortOn

-- | Split a list at the first element where predicate fails.
span :: TTerm (a -> Bool) -> TTerm [a] -> TTerm ([a], [a])
span = primitive2 _lists_span

-- | Get all elements of a list except the first.
tail :: TTerm [a] -> TTerm [a]
tail = primitive1 _lists_tail

-- | Take the first n elements from a list.
take :: TTerm Int -> TTerm [a] -> TTerm [a]
take = primitive2 _lists_take

-- | Transpose a list of lists.
transpose :: TTerm [[a]] -> TTerm [[a]]
transpose = primitive1 _lists_transpose

-- | Zip two lists into pairs.
zip :: TTerm [a] -> TTerm [b] -> TTerm [(a, b)]
zip = primitive2 _lists_zip

-- | Zip two lists with a combining function.
zipWith :: TTerm (a -> b -> c) -> TTerm [a] -> TTerm [b] -> TTerm [c]
zipWith = primitive3 _lists_zipWith
