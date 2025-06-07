-- | Haskell implementations of hydra.lib.strings primitives

module Hydra.Lib.Strings where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


cat :: [String] -> String
cat = L.concat

cat2 :: String -> String -> String
cat2 s1 s2 = s1 ++ s2

fromList :: [Int] -> String
fromList = fmap C.chr

intercalate :: String -> [String] -> String
intercalate = L.intercalate

isEmpty :: String -> Bool
isEmpty = L.null

length :: String -> Int
length = L.length

lines :: String -> [String]
lines = L.lines

splitOn :: String -> String -> [String]
splitOn = LS.splitOn

toList :: String -> [Int]
toList = fmap C.ord

toLower :: String -> String
toLower = fmap C.toLower

toUpper :: String -> String
toUpper = fmap C.toUpper

unlines :: [String] -> String
unlines = L.unlines
