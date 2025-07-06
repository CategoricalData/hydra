-- | Natural-language descriptions for hydra.mantle types

module Hydra.Describe.Mantle where

import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Display numeric precision as a string
precision :: (Mantle.Precision -> String)
precision x = case x of
  Mantle.PrecisionArbitrary -> "arbitrary-precision"
  Mantle.PrecisionBits v1 -> (Strings.cat [
    Literals.showInt32 v1,
    "-bit"])
