-- | Phantom-typed term DSL for the hydra.lib.chars library

module Hydra.Dsl.Meta.Lib.Chars where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Check whether a character is alphanumeric.
isAlphaNum :: TTerm Int -> TTerm Bool
isAlphaNum = primitive1 _chars_isAlphaNum

-- | Check whether a character is lowercase.
isLower :: TTerm Int -> TTerm Bool
isLower = primitive1 _chars_isLower

-- | Check whether a character is a whitespace character.
isSpace :: TTerm Int -> TTerm Bool
isSpace = primitive1 _chars_isSpace

-- | Check whether a character is uppercase.
isUpper :: TTerm Int -> TTerm Bool
isUpper = primitive1 _chars_isUpper

-- | Convert a character to lowercase.
toLower :: TTerm Int -> TTerm Int
toLower = primitive1 _chars_toLower

-- | Convert a character to uppercase.
toUpper :: TTerm Int -> TTerm Int
toUpper = primitive1 _chars_toUpper
