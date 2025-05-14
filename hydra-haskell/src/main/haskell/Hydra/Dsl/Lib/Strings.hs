module Hydra.Dsl.Lib.Strings where

import Hydra.Dsl.Phantoms
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms


(++) :: TTerm String -> TTerm String -> TTerm String
l ++ r = (primitive _strings_cat) @@ (list [l, r])

cat :: TTerm [String] -> TTerm String
cat = primitive1 _strings_cat

cat2 :: TTerm String -> TTerm String -> TTerm String
cat2 = primitive2 _strings_cat2

fromList :: TTerm [Int] -> TTerm String
fromList = primitive1 _strings_fromList

intercalate :: TTerm String -> TTerm [String] -> TTerm String
intercalate = primitive2 _strings_intercalate

isEmpty :: TTerm String -> TTerm Bool
isEmpty = primitive1 _strings_isEmpty

length :: TTerm String -> TTerm Int
length = primitive1 _strings_length

splitOn :: TTerm String -> TTerm String -> TTerm [String]
splitOn = primitive2 _strings_splitOn

toList :: TTerm String -> TTerm [Int]
toList = primitive1 _strings_toList

toLower :: TTerm String -> TTerm String
toLower = primitive1 _strings_toLower

toUpper :: TTerm String -> TTerm String
toUpper = primitive1 _strings_toUpper

-- Helpers

concat :: [TTerm String] -> TTerm String
concat strings = primitive _strings_cat @@ list strings
