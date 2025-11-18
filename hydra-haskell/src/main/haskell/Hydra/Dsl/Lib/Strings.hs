-- | Phantom-typed term DSL for the hydra.lib.strings library

module Hydra.Dsl.Lib.Strings where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


(++) :: TTerm String -> TTerm String -> TTerm String
l ++ r = (primitive _strings_cat) @@ (list [l, r])

cat :: TTerm [String] -> TTerm String
cat = primitive1 _strings_cat

cat2 :: TTerm String -> TTerm String -> TTerm String
cat2 = primitive2 _strings_cat2

charAt :: TTerm Int -> TTerm String -> TTerm Int
charAt = primitive2 _strings_charAt

fromList :: TTerm [Int] -> TTerm String
fromList = primitive1 _strings_fromList

intercalate :: TTerm String -> TTerm [String] -> TTerm String
intercalate = primitive2 _strings_intercalate

length :: TTerm String -> TTerm Int
length = primitive1 _strings_length

lines :: TTerm String -> TTerm [String]
lines = primitive1 _strings_lines

null :: TTerm String -> TTerm Bool
null = primitive1 _strings_null

splitOn :: TTerm String -> TTerm String -> TTerm [String]
splitOn = primitive2 _strings_splitOn

toList :: TTerm String -> TTerm [Int]
toList = primitive1 _strings_toList

toLower :: TTerm String -> TTerm String
toLower = primitive1 _strings_toLower

toUpper :: TTerm String -> TTerm String
toUpper = primitive1 _strings_toUpper

unlines :: TTerm [String] -> TTerm String
unlines = primitive1 _strings_unlines

-- Helpers

concat :: [TTerm String] -> TTerm String
concat strings = primitive _strings_cat @@ list strings
