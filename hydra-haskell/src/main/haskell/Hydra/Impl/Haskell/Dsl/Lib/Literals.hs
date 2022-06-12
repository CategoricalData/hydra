module Hydra.Impl.Haskell.Dsl.Lib.Literals where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries


showInt32 :: Data (Int -> String)
showInt32 = Data $ Terms.primitive _literals_showInt32

showString :: Data (String -> String)
showString = Data $ Terms.primitive _literals_showString
