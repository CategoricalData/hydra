module Hydra.Adapters.Utils where

import Hydra.Basics as Basics
import Hydra.Core as Core
import Hydra.Lib.Literals as Literals
import Hydra.Lib.Strings as Strings

-- Display a floating-point type as a string
describeFloatType :: (FloatType -> String)
describeFloatType t = (
  Strings.cat [
    describePrecision (floatTypePrecision t),
    " floating-point numbers"])

-- Display numeric precision as a string
describePrecision :: (Precision -> String)
describePrecision x = case x of
  PrecisionArbitrary -> "arbitrary-precision"
  PrecisionBits v -> (
    Strings.cat [
      Literals.showInt32 v,
      "-bit"])