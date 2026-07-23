-- | Haskell implementations of hydra.lib.lists primitives

module Hydra.Overlay.Haskell.Lib.Lists where

import Prelude hiding (compose, head, init, last, map, pure, tail, takeWhile)
import Hydra.Util
import Hydra.Core
import Hydra.Graph
import qualified Hydra.Overlay.Haskell.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Set as S


-- | Apply a list of functions to a list of values (applicative style).
apply :: [a -> b] -> [a] -> [b]
apply = (<*>)

-- | Return the element at the given index, or Nothing if out of bounds. (Alias of maybeAt.)
at :: Int -> [a] -> Maybe a
at i l
  | i < 0 || i >= L.length l = Nothing
  | otherwise = Just (l !! i)

-- | Apply a function that returns lists to each element and flatten results.
bind :: [a] -> (a -> [b]) -> [b]
bind = (>>=)

-- | Compose two list-returning functions (Kleisli composition in the list monad).
compose :: (a -> [b]) -> (b -> [c]) -> a -> [c]
compose f g x = f x >>= g

-- | Concatenate a list of lists.
concat :: [[a]] -> [a]
concat = L.concat

-- | Concatenate two lists.
concat2 :: [a] -> [a] -> [a]
concat2 l1 l2 = l1 ++ l2

-- | Prepend a value to a list.
cons :: a -> [a] -> [a]
cons = (:)

-- | Remove duplicate elements from a list. (Alias of nub.)
distinct :: Eq a => [a] -> [a]
distinct = L.nub

-- | Drop the first n elements from a list.
drop :: Int -> [a] -> [a]
drop = L.drop

-- | Drop elements from the beginning of a list while predicate is true.
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = L.dropWhile

-- | Filter a list based on a predicate.
filter :: (a -> Bool) -> [a] -> [a]
filter = L.filter

-- | Find the first element matching a predicate.
find :: (a -> Bool) -> [a] -> Maybe a
find = L.find

-- | Left-fold a list in the list monad (a nondeterministic fold).
foldList :: (a -> b -> [a]) -> a -> [b] -> [a]
foldList = CM.foldM

-- | Fold a list from the left.
-- Strict left fold. Lazy foldl can pile up thunks for accumulator types
-- that are field-update records (e.g. PythonModuleMetadata in
-- Hydra.Python.Coder.extendMetaForTerm), causing pathological wallclock
-- in the Python coder's gatherMetadata pass. Use foldl' to evaluate
-- each accumulator step strictly.
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = L.foldl'

-- | Fold a list from the right.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = L.foldr

-- | Group consecutive equal elements.
group :: Eq a => [a] -> [[a]]
group = L.group

-- | Return the first element, or Nothing if the list is empty. (Alias of maybeHead.)
head :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x

-- | Return all elements except the last, or Nothing if the list is empty. (Alias of maybeInit.)
init :: [a] -> Maybe [a]
init [] = Nothing
init xs = Just (L.init xs)

-- | Intercalate a list of lists with a separator list between each. (Alias of intercalate.)
join :: [a] -> [[a]] -> [a]
join = L.intercalate

-- | Intersperse a value between elements of a list.
intersperse :: a -> [a] -> [a]
intersperse = L.intersperse

-- | Get the length of a list.
length :: [a] -> Int
length = L.length

-- | Map a function over a list.
map :: (a -> b) -> [a] -> [b]
map = fmap

-- | Traverse a list in the list monad.
mapList :: (a -> [b]) -> [a] -> [[b]]
mapList = CM.mapM

-- | Traverse an optional value in the list monad.
mapOptional :: (a -> [b]) -> Maybe a -> [Maybe b]
mapOptional = traverse

-- | Traverse a set in the list monad.
mapSet :: Ord b => (a -> [b]) -> S.Set a -> [S.Set b]
mapSet f s = fmap S.fromList (CM.mapM f (S.toList s))

-- | Return the last element, or Nothing if the list is empty. (Alias of maybeLast.)
last :: [a] -> Maybe a
last [] = Nothing
last xs = Just (L.last xs)

-- | Test whether an element is in a list. (Alias of elem.)
member :: Eq a => a -> [a] -> Bool
member = L.elem

-- | Check if a list is empty.
null :: [a] -> Bool
null = L.null

-- | Partition a list into elements that satisfy a predicate and elements that do not.
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = L.partition

-- | Create a list with a single element.
pure :: a -> [a]
pure e = [e]

-- | Create a list with n copies of a value.
replicate :: Int -> a -> [a]
replicate = L.replicate

-- | Reverse a list.
reverse :: [a] -> [a]
reverse = L.reverse

-- | Create a single-element list.
singleton :: a -> [a]
singleton e = [e]

-- | Sort a list.
sort :: Ord a => [a] -> [a]
sort = L.sort

-- | Sort a list based on a key function. (Alias of sortOn.)
sortBy :: Ord b => (a -> b) -> [a] -> [a]
sortBy = L.sortOn

-- | Split a list at the first element where predicate fails.
span :: (a -> Bool) -> [a] -> ([a], [a])
span = L.span

-- | Return all elements except the first, or Nothing if the list is empty. (Alias of maybeTail.)
tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_:xs) = Just xs

-- | Take the first n elements from a list.
take :: Int -> [a] -> [a]
take = L.take

-- | Take elements from the beginning of a list while a predicate holds.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = L.takeWhile

-- | Transpose a list of lists.
transpose :: [[a]] -> [[a]]
transpose = L.transpose

-- | Decompose a list into its head and tail, returning Nothing if the list is empty.
uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

-- | Zip two lists into pairs.
zip :: [a] -> [b] -> [(a, b)]
zip = L.zip

-- | Zip two lists with a combining function.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = L.zipWith
