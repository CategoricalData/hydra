module Hydra.Lib.Lists where

import qualified Data.List as L


hsConcat :: [[a]] -> [a]
hsConcat = L.concat

hsLength :: [a] -> Int
hsLength = L.length

hsMap :: (a -> b) -> [a] -> [b]
hsMap = fmap
