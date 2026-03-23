-- | Phantom-typed term DSL for the hydra.lib.regex library

module Hydra.Dsl.Meta.Lib.Regex where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


find :: TTerm String -> TTerm String -> TTerm (Maybe String)
find = primitive2 _regex_find

findAll :: TTerm String -> TTerm String -> TTerm [String]
findAll = primitive2 _regex_findAll

matches :: TTerm String -> TTerm String -> TTerm Bool
matches = primitive2 _regex_matches

replace :: TTerm String -> TTerm String -> TTerm String -> TTerm String
replace = primitive3 _regex_replace

replaceAll :: TTerm String -> TTerm String -> TTerm String -> TTerm String
replaceAll = primitive3 _regex_replaceAll

split :: TTerm String -> TTerm String -> TTerm [String]
split = primitive2 _regex_split
