-- | Haskell implementations of hydra.lib.equality primitives. These simply make use of derived Eq.

module Hydra.Lib.Equality where

import Hydra.Core
import Hydra.Graph

import Data.Int


compare :: Ord a => a -> a -> Comparison
compare x y
  | x < y     = ComparisonLessThan
  | x > y     = ComparisonGreaterThan
  | otherwise = ComparisonEqualTo

equal :: Eq a => a -> a -> Bool
equal = (==)

gtFloat32 :: Float -> Float -> Bool
gtFloat32 = (>)

gtFloat64 :: Double -> Double -> Bool
gtFloat64 = (>)

gtInt32 :: Int -> Int -> Bool
gtInt32 = (>)

gteFloat32 :: Float -> Float -> Bool
gteFloat32 = (>=)

gteFloat64 :: Double -> Double -> Bool
gteFloat64 = (>=)

gteInt32 :: Int -> Int -> Bool
gteInt32 = (>=)

identity :: x -> x
identity = id

ltFloat32 :: Float -> Float -> Bool
ltFloat32 = (<)

ltFloat64 :: Double -> Double -> Bool
ltFloat64 = (<)

ltInt32 :: Int -> Int -> Bool
ltInt32 = (<)

lteFloat32 :: Float -> Float -> Bool
lteFloat32 = (<=)

lteFloat64 :: Double -> Double -> Bool
lteFloat64 = (<=)

lteInt32 :: Int -> Int -> Bool
lteInt32 = (<=)
