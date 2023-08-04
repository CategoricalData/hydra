-- | Haskell implementations of hydra/lib/lists primitives

module Hydra.Lib.Lists where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.List as L


apply :: [x -> y] -> [x] -> [y]
apply = (<*>)

bind :: [x] -> (x -> [y]) -> [y]
bind = (>>=)

concat :: [[x]] -> [x]
concat = L.concat

concat2 :: [x] -> [x] -> [x]
concat2 l1 l2 = l1 ++ l2

cons :: x -> [x] -> [x]
cons = (:)

head :: [x] -> x
head = L.head

intercalate :: [x] -> [[x]] -> [x]
intercalate = L.intercalate

intersperse :: x -> [x] -> [x]
intersperse = L.intersperse

last :: [x] -> x
last = L.last

length :: [x] -> Int
length = L.length

map :: (x -> y) -> [x] -> [y]
map = fmap

pure :: x -> [x]
pure e = [e]

reverse :: [x] -> [x]
reverse = L.reverse

tail :: [x] -> [x]
tail = L.tail
