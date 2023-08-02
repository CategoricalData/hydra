module Hydra.Dsl.Lib.Strings where

import Hydra.Dsl.Base
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


(++) :: Datum String -> Datum String -> Datum String
l ++ r = (Datum $ Terms.primitive _strings_cat) @@ (list [l, r])

cat :: Datum ([String] -> String)
cat = Datum $ Terms.primitive _strings_cat

cat2 :: Datum (String -> String -> String)
cat2 = Datum $ Terms.primitive _strings_cat2

fromList :: Datum ([Int] -> String)
fromList = Datum $ Terms.primitive _strings_fromList

intercalate :: Datum (String -> [String] -> String)
intercalate = Datum $ Terms.primitive _strings_intercalate

isEmpty :: Datum (String -> Bool)
isEmpty = Datum $ Terms.primitive _strings_isEmpty

length :: Datum (String -> Int)
length = Datum $ Terms.primitive _strings_length

splitOn :: Datum (String -> String -> [String])
splitOn = Datum $ Terms.primitive _strings_splitOn

toList :: Datum (String -> [Int])
toList = Datum $ Terms.primitive _strings_toList

toLower :: Datum (String -> String)
toLower = Datum $ Terms.primitive _strings_toLower

toUpper :: Datum (String -> String)
toUpper = Datum $ Terms.primitive _strings_toUpper
