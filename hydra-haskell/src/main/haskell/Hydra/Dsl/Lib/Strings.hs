module Hydra.Dsl.Lib.Strings where

import Hydra.Dsl.Base
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


(++) :: TTerm String -> TTerm String -> TTerm String
l ++ r = (TTerm $ Terms.primitive _strings_cat) @@ (list [l, r])

cat :: TTerm ([String] -> String)
cat = TTerm $ Terms.primitive _strings_cat

cat2 :: TTerm (String -> String -> String)
cat2 = TTerm $ Terms.primitive _strings_cat2

fromList :: TTerm ([Int] -> String)
fromList = TTerm $ Terms.primitive _strings_fromList

intercalate :: TTerm (String -> [String] -> String)
intercalate = TTerm $ Terms.primitive _strings_intercalate

isEmpty :: TTerm (String -> Bool)
isEmpty = TTerm $ Terms.primitive _strings_isEmpty

length :: TTerm (String -> Int)
length = TTerm $ Terms.primitive _strings_length

splitOn :: TTerm (String -> String -> [String])
splitOn = TTerm $ Terms.primitive _strings_splitOn

toList :: TTerm (String -> [Int])
toList = TTerm $ Terms.primitive _strings_toList

toLower :: TTerm (String -> String)
toLower = TTerm $ Terms.primitive _strings_toLower

toUpper :: TTerm (String -> String)
toUpper = TTerm $ Terms.primitive _strings_toUpper

-- Helpers

concat :: [TTerm String] -> TTerm String
concat strings = (TTerm $ Terms.primitive _strings_cat) @@ list strings
