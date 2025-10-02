--| Phantom-typed term DSL for the hydra.lib.chars library

module Hydra.Dsl.Lib.Chars where

import Hydra.Dsl.Phantoms
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


-- Follow's GHC.s Data.Char.isAlphaNum
isAlphaNum :: TTerm Int -> TTerm Bool
isAlphaNum = primitive1 _chars_isAlphaNum

-- Follows GHC's Data.Char.isLower (GHC.Internal.Unicode.isLower)
isLower :: TTerm Int -> TTerm Bool
isLower = primitive1 _chars_isLower

-- Follow's GHC.s Data.Char.isSpace
isSpace :: TTerm Int -> TTerm Bool
isSpace = primitive1 _chars_isSpace

-- Follows GHC's Data.Char.isUpper (GHC.Internal.Unicode.isUpper)
isUpper :: TTerm Int -> TTerm Bool
isUpper = primitive1 _chars_isUpper

-- Follows GHC's toLower (GHC.Internal.Unicode.Char.UnicodeData.SimpleLowerCaseMapping.toSimpleLowerCase)
toLower :: TTerm Int -> TTerm Int
toLower = primitive1 _chars_toLower

-- Follows GHC's toUpper (GHC.Internal.Unicode.Char.UnicodeData.SimpleUpperCaseMapping.toSimpleUpperCase)
toUpper :: TTerm Int -> TTerm Int
toUpper = primitive1 _chars_toUpper
