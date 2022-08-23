module Hydra.Lib.Lists where

import qualified Data.List as L


apply :: [a -> b] -> [a] -> [b]
apply = (<*>)

bind :: [a] -> (a -> [b]) -> [b]
bind = (>>=)

concat :: [[a]] -> [a]
concat = L.concat

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

pure :: a -> [a]
pure x = [x]
