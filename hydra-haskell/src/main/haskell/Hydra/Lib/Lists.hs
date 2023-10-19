-- | Haskell implementations of hydra/lib/lists primitives

module Hydra.Lib.Lists where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.List as L


apply :: [a -> b] -> [a] -> [b]
apply = (<*>)

bind :: [a] -> (a -> [b]) -> [b]
bind = (>>=)

concat :: [[a]] -> [a]
concat = L.concat

concat2 :: [a] -> [a] -> [a]
concat2 l1 l2 = l1 ++ l2

cons :: a -> [a] -> [a]
cons = (:)

head :: [a] -> a
head = L.head

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

reverse :: [a] -> [a]
reverse = L.reverse

tail :: [a] -> [a]
tail = L.tail
