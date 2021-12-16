module Hydra.Lib.Lists where

import qualified Data.List as L


concat :: [[a]] -> [a]
concat = L.concat

length :: [a] -> Int
length = L.length

map :: (a -> b) -> [a] -> [b]
map = fmap
