-- | Phantom-typed term DSL for the hydra.lib.strings library

module Hydra.Dsl.Meta.Lib.Strings where

import Hydra.Typed
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Prims as Prims
import qualified Hydra.Lib.Strings as DefStrings


-- | Concatenate a list of strings into a single string.
cat :: TypedTerm [String] -> TypedTerm String
cat = primitive1 DefStrings.cat

-- | Concatenate two strings.
cat2 :: TypedTerm String -> TypedTerm String -> TypedTerm String
cat2 = primitive2 DefStrings.cat2

-- | Convert a list of Unicode code points to a string.
fromList :: TypedTerm [Int] -> TypedTerm String
fromList = primitive1 DefStrings.fromList

-- | Join a list of strings with a separator between each element.
intercalate :: TypedTerm String -> TypedTerm [String] -> TypedTerm String
intercalate = primitive2 DefStrings.intercalate

-- | Return the length of a string.
length :: TypedTerm String -> TypedTerm Int
length = primitive1 DefStrings.length

-- | Split a string into lines.
lines :: TypedTerm String -> TypedTerm [String]
lines = primitive1 DefStrings.lines

-- | Get the Unicode code point of the character at a specific index, returning Nothing if out of bounds.
maybeCharAt :: TypedTerm Int -> TypedTerm String -> TypedTerm (Maybe Int)
maybeCharAt = primitive2 DefStrings.maybeCharAt

-- | Check whether a string is empty.
null :: TypedTerm String -> TypedTerm Bool
null = primitive1 DefStrings.null

-- | Split a string on a delimiter string.
splitOn :: TypedTerm String -> TypedTerm String -> TypedTerm [String]
splitOn = primitive2 DefStrings.splitOn

-- | Convert a string to a list of Unicode code points.
toList :: TypedTerm String -> TypedTerm [Int]
toList = primitive1 DefStrings.toList

-- | Convert a string to lowercase.
toLower :: TypedTerm String -> TypedTerm String
toLower = primitive1 DefStrings.toLower

-- | Convert a string to uppercase.
toUpper :: TypedTerm String -> TypedTerm String
toUpper = primitive1 DefStrings.toUpper

-- | Join a list of strings with newlines, appending a trailing newline.
unlines :: TypedTerm [String] -> TypedTerm String
unlines = primitive1 DefStrings.unlines

-- Helpers

-- | Concatenate a Haskell list of string terms into a single string.
concat :: [TypedTerm String] -> TypedTerm String
concat strings = primitive DefStrings.cat @@ list strings
