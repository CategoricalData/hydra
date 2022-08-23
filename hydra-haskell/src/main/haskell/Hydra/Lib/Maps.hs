module Hydra.Lib.Maps where

import qualified Data.Map as M


map :: (v1 -> v2) -> M.Map k v1 -> M.Map k v2
map = fmap

size :: M.Map k v -> Int
size = M.size
