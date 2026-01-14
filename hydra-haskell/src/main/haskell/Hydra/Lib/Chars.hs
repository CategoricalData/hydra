-- | Haskell implementations of hydra.lib.chars primitives.
-- All functions operate on Unicode code points (Int) rather than Char.

module Hydra.Lib.Chars where

import qualified Data.Char as C


isAlphaNum :: Int -> Bool
isAlphaNum = C.isAlphaNum . C.chr

isLower :: Int -> Bool
isLower = C.isLower . C.chr

isSpace :: Int -> Bool
isSpace = C.isSpace . C.chr

isUpper :: Int -> Bool
isUpper = C.isUpper . C.chr

toLower :: Int -> Int
toLower = C.ord . C.toLower . C.chr

toUpper :: Int -> Int
toUpper = C.ord . C.toUpper . C.chr
