-- | Haskell implementations of hydra.lib.lists primitives

module Hydra.Lib.Lists where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.List as L


apply :: [a -> b] -> [a] -> [b]
apply = (<*>)

at :: Int -> [a] -> a
at i l = l !! i

bind :: [a] -> (a -> [b]) -> [b]
bind = (>>=)

concat :: [[a]] -> [a]
concat = L.concat

concat2 :: [a] -> [a] -> [a]
concat2 l1 l2 = l1 ++ l2

cons :: a -> [a] -> [a]
cons = (:)

drop :: Int -> [a] -> [a]
drop = L.drop

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = L.dropWhile

elem :: Eq a => a -> [a] -> Bool
elem = L.elem

filter :: (a -> Bool) -> [a] -> [a]
filter = L.filter

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = L.foldl

group :: Eq a => [a] -> [[a]]
group = L.group

head :: [a] -> a
head = L.head

init :: [a] -> [a]
init = L.init

intercalate :: [a] -> [[a]] -> [a]
intercalate = L.intercalate

intersperse :: a -> [a] -> [a]
intersperse = L.intersperse

last :: [a] -> a
last = L.last

length :: [a] -> Int
length = L.length

map :: (a -> b) -> [a] -> [b]
map = fmap

nub :: Eq a => [a] -> [a]
nub = L.nub

null :: [a] -> Bool
null = L.null

pure :: a -> [a]
pure e = [e]

replicate :: Int -> a -> [a]
replicate = L.replicate

reverse :: [a] -> [a]
reverse = L.reverse

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

singleton :: a -> [a]
singleton e = [e]

sort :: Ord a => [a] -> [a]
sort = L.sort

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = L.sortOn

span :: (a -> Bool) -> [a] -> ([a], [a])
span = L.span

tail :: [a] -> [a]
tail = L.tail

take :: Int -> [a] -> [a]
take = L.take

transpose :: [[a]] -> [[a]]
transpose = L.transpose

zip :: [a] -> [b] -> [(a, b)]
zip = L.zip

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = L.zipWith
