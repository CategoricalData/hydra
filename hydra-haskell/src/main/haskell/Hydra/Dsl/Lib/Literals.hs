module Hydra.Dsl.Lib.Literals where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


showInt32 :: Datum (Int -> String)
showInt32 = Datum $ Terms.primitive _literals_showInt32

showString :: Datum (String -> String)
showString = Datum $ Terms.primitive _literals_showString
