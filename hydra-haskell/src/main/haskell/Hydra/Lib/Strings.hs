-- | Haskell implementations of hydra.lib.strings primitives

module Hydra.Lib.Strings where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


-- | Concatenate a list of strings.
cat :: [String] -> String
cat = L.concat

-- | Concatenate two strings.
cat2 :: String -> String -> String
cat2 s1 s2 = s1 ++ s2

-- | Get the character code at a specific index in a string.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
charAt :: Int -> String -> Int
charAt i s = C.ord (s !! i)

-- | Convert a list of Unicode code points to a string.
fromList :: [Int] -> String
fromList = fmap C.chr

-- | Intercalate a string between a list of strings.
intercalate :: String -> [String] -> String
intercalate = L.intercalate

-- | Return the length of a string.
length :: String -> Int
length = L.length

-- | Split a string into lines.
lines :: String -> [String]
lines = L.lines

-- | Check if a string is null/empty.
null :: String -> Bool
null = L.null

-- | Split a string on a delimiter.
splitOn :: String -> String -> [String]
splitOn = LS.splitOn

-- | Convert a string to a list of Unicode code points.
toList :: String -> [Int]
toList = fmap C.ord

-- | Convert a string to lowercase.
toLower :: String -> String
toLower = fmap C.toLower

-- | Convert a string to uppercase.
toUpper :: String -> String
toUpper = fmap C.toUpper

-- | Join strings with newlines, adding trailing newline.
unlines :: [String] -> String
unlines = L.unlines
