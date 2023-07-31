-- | Haskell implementations of hydra/lib/strings primitives

module Hydra.Lib.Strings where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


cat :: [String] -> String
cat = L.concat

fromList :: [Int] -> String
fromList = fmap C.chr

isEmpty :: String -> Bool
isEmpty = L.null

length :: String -> Int
length = L.length

splitOn :: String -> String -> [String]
splitOn = LS.splitOn

toList :: String -> [Int]
toList = fmap C.ord

toLower :: String -> String
toLower = fmap C.toLower

toUpper :: String -> String
toUpper = fmap C.toUpper
