module Hydra.Impl.Haskell.Dsl.Lib.Strings where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries


cat :: Data ([String] -> String)
cat = Data $ Terms.primitive _strings_cat

length :: Data (String -> Int)
length = Data $ Terms.primitive _strings_length

splitOn :: Data (String -> String -> [String])
splitOn = Data $ Terms.primitive _strings_splitOn

toLower :: Data (String -> String)
toLower = Data $ Terms.primitive _strings_toLower

toUpper :: Data (String -> String)
toUpper = Data $ Terms.primitive _strings_toUpper
