module Hydra.Dsl.Lib.Strings where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


cat :: Datum ([String] -> String)
cat = Datum $ Terms.primitive _strings_cat

fromList :: Datum ([Int] -> String)
fromList = Datum $ Terms.primitive _strings_fromList

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
