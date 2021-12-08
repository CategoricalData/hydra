module Hydra.Adapters.Utils where

import Hydra.Core
import Hydra.Lib.Literals
import Hydra.Lib.Strings

-- Display numeric precision as a string
describePrecision :: (Precision -> String)
describePrecision x = case x of
  PrecisionArbitrary -> "arbitrary-precision"
  PrecisionBits v -> (hsCat (hsShowInt32 v) "-bit")
