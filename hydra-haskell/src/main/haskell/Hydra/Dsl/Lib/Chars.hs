module Hydra.Dsl.Lib.Chars where

import Hydra.Dsl.Base
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


-- Follow's GHC.s Data.Char.isAlphaNum
isAlphaNum :: TTerm (Int -> Bool)
isAlphaNum = TTerm $ Terms.primitive _chars_isAlphaNum

-- Follows GHC's Data.Char.isLower (GHC.Internal.Unicode.isLower)
isLower :: TTerm (Int -> Bool)
isLower = TTerm $ Terms.primitive _chars_isLower

-- Follow's GHC.s Data.Char.isSpace
isSpace :: TTerm (Int -> Bool)
isSpace = TTerm $ Terms.primitive _chars_isSpace

-- Follows GHC's Data.Char.isUpper (GHC.Internal.Unicode.isUpper)
isUpper :: TTerm (Int -> Bool)
isUpper = TTerm $ Terms.primitive _chars_isUpper

-- Follows GHC's toLower (GHC.Internal.Unicode.Char.UnicodeData.SimpleLowerCaseMapping.toSimpleLowerCase)
toLower :: TTerm (Int -> Int)
toLower = TTerm $ Terms.primitive _chars_toLower

-- Follows GHC's toUpper (GHC.Internal.Unicode.Char.UnicodeData.SimpleUpperCaseMapping.toSimpleUpperCase)
toUpper :: TTerm (Int -> Int)
toUpper = TTerm $ Terms.primitive _chars_toUpper
