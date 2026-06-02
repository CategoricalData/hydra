-- | Phantom-typed term DSL for the hydra.lib.chars library

module Hydra.Dsl.Meta.Lib.Chars where

import Hydra.Typed
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Check whether a character is alphanumeric.
isAlphaNum :: TypedTerm Int -> TypedTerm Bool
isAlphaNum = primitive1 _chars_isAlphaNum

-- | Check whether a character is lowercase.
isLower :: TypedTerm Int -> TypedTerm Bool
isLower = primitive1 _chars_isLower

-- | Check whether a character is a whitespace character.
isSpace :: TypedTerm Int -> TypedTerm Bool
isSpace = primitive1 _chars_isSpace

-- | Check whether a character is uppercase.
isUpper :: TypedTerm Int -> TypedTerm Bool
isUpper = primitive1 _chars_isUpper

-- | Convert a character to lowercase.
toLower :: TypedTerm Int -> TypedTerm Int
toLower = primitive1 _chars_toLower

-- | Convert a character to uppercase.
toUpper :: TypedTerm Int -> TypedTerm Int
toUpper = primitive1 _chars_toUpper
