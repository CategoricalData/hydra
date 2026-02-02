-- | Haskell implementations of hydra.lib.lists primitives

module Hydra.Lib.Lists where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.List as L


-- | Apply a list of functions to a list of values (applicative style).
apply :: [a -> b] -> [a] -> [b]
apply = (<*>)

-- | Get the element at specified index of a list.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
at :: Int -> [a] -> a
at i l = l !! i

-- | Apply a function that returns lists to each element and flatten results.
bind :: [a] -> (a -> [b]) -> [b]
bind = (>>=)

-- | Concatenate a list of lists.
concat :: [[a]] -> [a]
concat = L.concat

-- | Concatenate two lists.
concat2 :: [a] -> [a] -> [a]
concat2 l1 l2 = l1 ++ l2

-- | Prepend a value to a list.
cons :: a -> [a] -> [a]
cons = (:)

-- | Drop the first n elements from a list.
drop :: Int -> [a] -> [a]
drop = L.drop

-- | Drop elements from the beginning of a list while predicate is true.
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = L.dropWhile

-- | Check if an element is in a list.
elem :: Eq a => a -> [a] -> Bool
elem = L.elem

-- | Filter a list based on a predicate.
filter :: (a -> Bool) -> [a] -> [a]
filter = L.filter

-- | Find the first element matching a predicate.
find :: (a -> Bool) -> [a] -> Maybe a
find = L.find

-- | Fold a list from the left.
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = L.foldl

-- | Group consecutive equal elements.
group :: Eq a => [a] -> [[a]]
group = L.group

-- | Get the first element of a list.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
head :: [a] -> a
head = L.head

-- | Return all elements except the last one.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
init :: [a] -> [a]
init = L.init

-- | Intercalate a list of lists with a separator list between each.
intercalate :: [a] -> [[a]] -> [a]
intercalate = L.intercalate

-- | Intersperse a value between elements of a list.
intersperse :: a -> [a] -> [a]
intersperse = L.intersperse

-- | Get the last element of a list.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
last :: [a] -> a
last = L.last

-- | Get the length of a list.
length :: [a] -> Int
length = L.length

-- | Map a function over a list.
map :: (a -> b) -> [a] -> [b]
map = fmap

-- | Remove duplicate elements from a list.
nub :: Eq a => [a] -> [a]
nub = L.nub

-- | Check if a list is empty.
null :: [a] -> Bool
null = L.null

-- | Create a list with a single element.
pure :: a -> [a]
pure e = [e]

-- | Partition a list based on a predicate.
-- Returns (elements satisfying predicate, elements not satisfying predicate).
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = L.partition

-- | Create a list with n copies of a value.
replicate :: Int -> a -> [a]
replicate = L.replicate

-- | Reverse a list.
reverse :: [a] -> [a]
reverse = L.reverse

-- | Get the first element of a list, returning Nothing if the list is empty.
-- TODO: consider renaming. See https://github.com/CategoricalData/hydra/issues/201
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Create a single-element list.
singleton :: a -> [a]
singleton e = [e]

-- | Sort a list.
sort :: Ord a => [a] -> [a]
sort = L.sort

-- | Sort a list based on a key function.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = L.sortOn

-- | Split a list at the first element where predicate fails.
span :: (a -> Bool) -> [a] -> ([a], [a])
span = L.span

-- | Get all elements of a list except the first.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
tail :: [a] -> [a]
tail = L.tail

-- | Take the first n elements from a list.
take :: Int -> [a] -> [a]
take = L.take

-- | Transpose a list of lists.
transpose :: [[a]] -> [[a]]
transpose = L.transpose

-- | Zip two lists into pairs.
zip :: [a] -> [b] -> [(a, b)]
zip = L.zip

-- | Zip two lists with a combining function.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = L.zipWith
