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

-- | An error of any kind, with kernel errors particularly differentiated
data Error = 
  -- | An error that occurred during decoding of a term
  ErrorDecoding DecodingError |
  -- | Any other error
  ErrorOther OtherError |
  -- | A type unification error
  ErrorUnification UnificationError
  deriving (Eq, Ord, Read, Show)

_Error = (Core.Name "hydra.error.Error")

_Error_decoding = (Core.Name "decoding")

_Error_other = (Core.Name "other")

_Error_unification = (Core.Name "unification")

-- | Any other error
newtype OtherError = 
  OtherError {
    unOtherError :: String}
  deriving (Eq, Ord, Read, Show)

_OtherError = (Core.Name "hydra.error.OtherError")

-- | An error that occurred during type unification
data UnificationError = 
  UnificationError {
    -- | The left-hand type in the unification
    unificationErrorLeftType :: Core.Type,
    -- | The right-hand type in the unification
    unificationErrorRightType :: Core.Type,
    -- | A human-readable error message
    unificationErrorMessage :: String}
  deriving (Eq, Ord, Read, Show)

_UnificationError = (Core.Name "hydra.error.UnificationError")

_UnificationError_leftType = (Core.Name "leftType")

_UnificationError_rightType = (Core.Name "rightType")

_UnificationError_message = (Core.Name "message")
