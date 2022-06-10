module Hydra.Impl.Haskell.Dsl.Lib.Literals where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries


showInt32 :: Trm (Int -> String)
showInt32 = Trm $ Terms.primitive _literals_showInt32

showString :: Trm (String -> String)
showString = Trm $ Terms.primitive _literals_showString
