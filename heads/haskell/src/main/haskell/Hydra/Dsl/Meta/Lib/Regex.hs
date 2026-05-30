-- | Phantom-typed term DSL for the hydra.lib.regex library

module Hydra.Dsl.Meta.Lib.Regex where

import Hydra.Typed
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Find the first substring matching a regex pattern.
find :: TypedTerm String -> TypedTerm String -> TypedTerm (Maybe String)
find = primitive2 _regex_find

-- | Find all non-overlapping substrings matching a regex pattern.
findAll :: TypedTerm String -> TypedTerm String -> TypedTerm [String]
findAll = primitive2 _regex_findAll

-- | Check whether an entire string matches a regex pattern.
matches :: TypedTerm String -> TypedTerm String -> TypedTerm Bool
matches = primitive2 _regex_matches

-- | Replace the first occurrence of a regex pattern with a replacement string.
replace :: TypedTerm String -> TypedTerm String -> TypedTerm String -> TypedTerm String
replace = primitive3 _regex_replace

-- | Replace all non-overlapping occurrences of a regex pattern with a replacement string.
replaceAll :: TypedTerm String -> TypedTerm String -> TypedTerm String -> TypedTerm String
replaceAll = primitive3 _regex_replaceAll

-- | Split a string by a regex pattern.
split :: TypedTerm String -> TypedTerm String -> TypedTerm [String]
split = primitive2 _regex_split
