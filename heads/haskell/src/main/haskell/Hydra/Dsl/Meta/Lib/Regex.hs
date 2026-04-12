-- | Phantom-typed term DSL for the hydra.lib.regex library

module Hydra.Dsl.Meta.Lib.Regex where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Find the first substring matching a regex pattern.
find :: TTerm String -> TTerm String -> TTerm (Maybe String)
find = primitive2 _regex_find

-- | Find all non-overlapping substrings matching a regex pattern.
findAll :: TTerm String -> TTerm String -> TTerm [String]
findAll = primitive2 _regex_findAll

-- | Check whether an entire string matches a regex pattern.
matches :: TTerm String -> TTerm String -> TTerm Bool
matches = primitive2 _regex_matches

-- | Replace the first occurrence of a regex pattern with a replacement string.
replace :: TTerm String -> TTerm String -> TTerm String -> TTerm String
replace = primitive3 _regex_replace

-- | Replace all non-overlapping occurrences of a regex pattern with a replacement string.
replaceAll :: TTerm String -> TTerm String -> TTerm String -> TTerm String
replaceAll = primitive3 _regex_replaceAll

-- | Split a string by a regex pattern.
split :: TTerm String -> TTerm String -> TTerm [String]
split = primitive2 _regex_split
