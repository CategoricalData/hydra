-- | Phantom-typed term DSL for the hydra.lib.strings library

module Hydra.Dsl.Meta.Lib.Strings where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Concatenate a list of strings into a single string.
cat :: TTerm [String] -> TTerm String
cat = primitive1 _strings_cat

-- | Concatenate two strings.
cat2 :: TTerm String -> TTerm String -> TTerm String
cat2 = primitive2 _strings_cat2

-- | Convert a list of Unicode code points to a string.
fromList :: TTerm [Int] -> TTerm String
fromList = primitive1 _strings_fromList

-- | Join a list of strings with a separator between each element.
intercalate :: TTerm String -> TTerm [String] -> TTerm String
intercalate = primitive2 _strings_intercalate

-- | Return the length of a string.
length :: TTerm String -> TTerm Int
length = primitive1 _strings_length

-- | Get the Unicode code point of the character at a specific index, returning Nothing if out of bounds.
maybeCharAt :: TTerm Int -> TTerm String -> TTerm (Maybe Int)
maybeCharAt = primitive2 _strings_maybeCharAt

-- | Split a string into lines.
lines :: TTerm String -> TTerm [String]
lines = primitive1 _strings_lines

-- | Check whether a string is empty.
null :: TTerm String -> TTerm Bool
null = primitive1 _strings_null

-- | Split a string on a delimiter string.
splitOn :: TTerm String -> TTerm String -> TTerm [String]
splitOn = primitive2 _strings_splitOn

-- | Convert a string to a list of Unicode code points.
toList :: TTerm String -> TTerm [Int]
toList = primitive1 _strings_toList

-- | Convert a string to lowercase.
toLower :: TTerm String -> TTerm String
toLower = primitive1 _strings_toLower

-- | Convert a string to uppercase.
toUpper :: TTerm String -> TTerm String
toUpper = primitive1 _strings_toUpper

-- | Join a list of strings with newlines, appending a trailing newline.
unlines :: TTerm [String] -> TTerm String
unlines = primitive1 _strings_unlines

-- Helpers

-- | Concatenate a Haskell list of string terms into a single string.
concat :: [TTerm String] -> TTerm String
concat strings = primitive _strings_cat @@ list strings
