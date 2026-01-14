-- | Haskell implementations of hydra.lib.chars primitives.
-- All functions operate on Unicode code points (Int) rather than Char.

module Hydra.Lib.Chars where

import qualified Data.Char as C


-- | Check if a character (as int) is alphanumeric.
isAlphaNum :: Int -> Bool
isAlphaNum = C.isAlphaNum . C.chr

-- | Check if a character (as int) is lowercase.
isLower :: Int -> Bool
isLower = C.isLower . C.chr

-- | Check if a character (as int) is whitespace.
isSpace :: Int -> Bool
isSpace = C.isSpace . C.chr

-- | Check if a character (as int) is uppercase.
isUpper :: Int -> Bool
isUpper = C.isUpper . C.chr

-- | Convert a character (as int) to lowercase, returning the int value.
toLower :: Int -> Int
toLower = C.ord . C.toLower . C.chr

-- | Convert a character (as int) to uppercase, returning the int value.
toUpper :: Int -> Int
toUpper = C.ord . C.toUpper . C.chr
