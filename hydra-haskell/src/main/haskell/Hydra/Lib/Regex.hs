-- | Haskell implementations of hydra.lib.regex primitives

module Hydra.Lib.Regex where

import qualified Text.Regex.TDFA as R


-- | Check whether an entire string matches a regex pattern.
matches :: String -> String -> Bool
matches pattern input = input R.=~ ("^(" ++ pattern ++ ")$" :: String) :: Bool

-- | Find the first substring matching a regex pattern.
find :: String -> String -> Maybe String
find pattern input = case (input R.=~ pattern :: (String, String, String)) of
  (_, match, _)
    | null match -> Nothing
    | otherwise  -> Just match

-- | Find all non-overlapping substrings matching a regex pattern.
findAll :: String -> String -> [String]
findAll pattern input = R.getAllTextMatches (input R.=~ pattern :: R.AllTextMatches [] String)

-- | Replace the first occurrence of a regex pattern with a replacement string.
replace :: String -> String -> String -> String
replace pattern replacement input = case (input R.=~ pattern :: (String, String, String)) of
  (before, match, after)
    | null match -> input
    | otherwise  -> before ++ replacement ++ after

-- | Replace all non-overlapping occurrences of a regex pattern with a replacement string.
replaceAll :: String -> String -> String -> String
replaceAll pattern replacement input = go input
  where
    go "" = ""
    go s = case (s R.=~ pattern :: (String, String, String)) of
      (before, match, after)
        | null match -> s
        | otherwise  -> before ++ replacement ++ go after

-- | Split a string by a regex pattern.
split :: String -> String -> [String]
split pattern input = go input
  where
    go "" = [""]
    go s = case (s R.=~ pattern :: (String, String, String)) of
      (before, match, after)
        | null match -> [s]
        | otherwise  -> before : go after
