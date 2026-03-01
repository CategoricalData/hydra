-- Note: this is an automatically generated file. Do not edit.

-- | Error types specific to the Hydra kernel

module Hydra.Error where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An error that occurred during decoding of a term
newtype DecodingError = 
  DecodingError {
    unDecodingError :: String}
  deriving (Eq, Ord, Read, Show)

_DecodingError = (Core.Name "hydra.error.DecodingError")
