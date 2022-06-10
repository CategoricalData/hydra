module Hydra.Impl.Haskell.Dsl.Lib.Strings where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries


cat :: Trm (String -> String -> String)
cat = Trm $ Terms.primitive _strings_cat

length :: Trm (String -> Int)
length = Trm $ Terms.primitive _strings_length

splitOn :: Trm (String -> String -> [String])
splitOn = Trm $ Terms.primitive _strings_splitOn

toLower :: Trm (String -> String)
toLower = Trm $ Terms.primitive _strings_toLower

toUpper :: Trm (String -> String)
toUpper = Trm $ Terms.primitive _strings_toUpper
