-- | Haskell implementations of hydra.lib.chars primitives.
-- All functions operate on Unicode code points (Int) rather than Char.

module Hydra.Lib.Chars where

import qualified Data.Char as C


-- | Check whether a character is alphanumeric.
isAlphaNum :: Int -> Bool
isAlphaNum = C.isAlphaNum . C.chr

-- | Check whether a character is lowercase.
isLower :: Int -> Bool
isLower = C.isLower . C.chr

-- | Check whether a character is a whitespace character.
isSpace :: Int -> Bool
isSpace = C.isSpace . C.chr

-- | Check whether a character is uppercase.
isUpper :: Int -> Bool
isUpper = C.isUpper . C.chr

-- | Convert a character to lowercase.
toLower :: Int -> Int
toLower = C.ord . C.toLower . C.chr

-- | Convert a character to uppercase.
toUpper :: Int -> Int
toUpper = C.ord . C.toUpper . C.chr
