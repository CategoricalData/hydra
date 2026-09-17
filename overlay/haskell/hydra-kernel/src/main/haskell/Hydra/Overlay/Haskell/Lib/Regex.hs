-- | Haskell implementations of hydra.lib.regex primitives.
--
-- Patterns are Hydra-defined and translingual (docs/specification/regex.md): each primitive first
-- runs the pattern through hydra.parse.regex, then renders the resulting AST to POSIX ERE syntax via
-- hydra.print.posix.regex (Haskell's Text.Regex.TDFA is POSIX ERE), and only then hands the rendered
-- pattern to the native engine. An ill-formed pattern (one that hydra.parse.regex rejects) is treated
-- as "no match" — the same portable-failure convention as an empty match, so no primitive here raises
-- an error for a malformed pattern; see issue #603.

module Hydra.Overlay.Haskell.Lib.Regex where

import qualified Text.Regex.TDFA as R

import qualified Hydra.Parse.Regex as ParseRegex
import qualified Hydra.Print.Posix.Regex as PrintPosixRegex


-- | Translate a Hydra regex pattern to POSIX ERE syntax via parse.regex |> print.posix.regex. Nothing
-- if the pattern does not parse (ill-formed).
toNative :: String -> Maybe String
toNative pattern = PrintPosixRegex.printRegex <$> ParseRegex.parseRegex pattern

-- | Find the first substring matching a regex pattern.
find :: String -> String -> Maybe String
find pattern input = case toNative pattern of
  Nothing -> Nothing
  Just native -> case (input R.=~ native :: (String, String, String)) of
    (_, match, _)
      | null match -> Nothing
      | otherwise  -> Just match

-- | Find all non-overlapping substrings matching a regex pattern.
findAll :: String -> String -> [String]
findAll pattern input = case toNative pattern of
  Nothing -> []
  Just native -> R.getAllTextMatches (input R.=~ native :: R.AllTextMatches [] String)

-- | Check whether an entire string matches a regex pattern.
matches :: String -> String -> Bool
matches pattern input = case toNative pattern of
  Nothing -> False
  Just native -> input R.=~ ("^(" ++ native ++ ")$" :: String) :: Bool

-- | Replace the first occurrence of a regex pattern with a replacement string.
replace :: String -> String -> String -> String
replace pattern replacement input = case toNative pattern of
  Nothing -> input
  Just native -> case (input R.=~ native :: (String, String, String)) of
    (before, match, after)
      | null match -> input
      | otherwise  -> before ++ replacement ++ after

-- | Replace all non-overlapping occurrences of a regex pattern with a replacement string.
replaceAll :: String -> String -> String -> String
replaceAll pattern replacement input = case toNative pattern of
  Nothing -> input
  Just native -> go input
    where
      go "" = ""
      go s = case (s R.=~ native :: (String, String, String)) of
        (before, match, after)
          | null match -> s
          | otherwise  -> before ++ replacement ++ go after

-- | Split a string by a regex pattern.
split :: String -> String -> [String]
split pattern input = case toNative pattern of
  Nothing -> [input]
  Just native -> go input
    where
      go "" = [""]
      go s = case (s R.=~ native :: (String, String, String)) of
        (before, match, after)
          | null match -> [s]
          | otherwise  -> before : go after
