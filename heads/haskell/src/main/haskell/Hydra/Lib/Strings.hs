-- | Haskell implementations of hydra.lib.strings primitives

module Hydra.Lib.Strings where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


-- | Concatenate a list of strings into a single string.
cat :: [String] -> String
cat = L.concat

-- | Concatenate two strings.
cat2 :: String -> String -> String
cat2 s1 s2 = s1 ++ s2

-- Bootstrap stub: removed in Phase 2.4 but kept so stale dist modules
-- continue to compile during Step 1. Not registered as a primitive.
charAt :: Int -> String -> Int
charAt i s = C.ord (s !! i)

-- | Convert a list of Unicode code points to a string.
fromList :: [Int] -> String
fromList = fmap C.chr

-- | Join a list of strings with a separator between each element.
intercalate :: String -> [String] -> String
intercalate = L.intercalate

-- | Return the length of a string.
length :: String -> Int
length = L.length

-- | Get the Unicode code point of the character at a specific index, returning Nothing if out of bounds.
maybeCharAt :: Int -> String -> Maybe Int
maybeCharAt i s
  | i < 0 || i >= L.length s = Nothing
  | otherwise = Just (C.ord (s !! i))

-- | Split a string into lines.
lines :: String -> [String]
lines = L.lines

-- | Check whether a string is empty.
null :: String -> Bool
null = L.null

-- | Split a string on a delimiter string.
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

-- | Join a list of strings with newlines, appending a trailing newline.
unlines :: [String] -> String
unlines = L.unlines
