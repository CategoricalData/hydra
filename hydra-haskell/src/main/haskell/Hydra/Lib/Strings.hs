-- | Haskell implementations of hydra.lib.strings primitives

module Hydra.Lib.Strings where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


cat :: [String] -> String
cat = L.concat

cat2 :: String -> String -> String
cat2 s1 s2 = s1 ++ s2

-- | Get the Unicode code point at the given index. Implements hydra.lib.strings.charAt.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
charAt :: Int -> String -> Int
charAt i s = C.ord (s !! i)

-- | Convert a list of Unicode code points to a string. Implements hydra.lib.strings.fromList.
fromList :: [Int] -> String
fromList = fmap C.chr

intercalate :: String -> [String] -> String
intercalate = L.intercalate

length :: String -> Int
length = L.length

lines :: String -> [String]
lines = L.lines

null :: String -> Bool
null = L.null

splitOn :: String -> String -> [String]
splitOn = LS.splitOn

-- | Convert a string to a list of Unicode code points. Implements hydra.lib.strings.toList.
toList :: String -> [Int]
toList = fmap C.ord

toLower :: String -> String
toLower = fmap C.toLower

toUpper :: String -> String
toUpper = fmap C.toUpper

unlines :: [String] -> String
unlines = L.unlines
