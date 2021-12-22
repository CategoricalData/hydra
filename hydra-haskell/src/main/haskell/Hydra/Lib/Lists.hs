module Hydra.Lib.Lists where

import qualified Data.List as L


concat :: [[a]] -> [a]
concat = L.concat

head :: [a] -> a
head = L.head

intercalate :: [a] -> [[a]] -> [a]
intercalate = L.intercalate

last :: [a] -> a
last = L.last

length :: [a] -> Int
length = L.length

map :: (a -> b) -> [a] -> [b]
map = fmap
