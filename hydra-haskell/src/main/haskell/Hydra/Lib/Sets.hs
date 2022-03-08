module Hydra.Lib.Sets where

import qualified Data.Set as S

add :: Ord a => a -> S.Set a -> S.Set a
add = S.insert

contains :: Ord a => a -> S.Set a -> Bool
contains = S.member

isEmpty :: S.Set a -> Bool
isEmpty = S.null

remove :: Ord a => a -> S.Set a -> S.Set a
remove = S.delete
