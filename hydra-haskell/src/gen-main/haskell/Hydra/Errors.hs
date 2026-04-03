-- Note: this is an automatically generated file. Do not edit.

-- | Top-level error types for the Hydra kernel

module Hydra.Errors where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as Core_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | An error that occurred during decoding of a term
newtype DecodingError =
  DecodingError {
    unDecodingError :: String}
  deriving (Eq, Ord, Read, Show)

_DecodingError = Core.Name "hydra.errors.DecodingError"

-- | An error of any kind, with kernel errors particularly differentiated
data Error =
  -- | A type checking error
  ErrorChecking Checking.CheckingError |
  -- | An error that occurred during decoding of a term
  ErrorDecoding DecodingError |
  -- | A duplicate binding name error
  ErrorDuplicateBinding Core_.DuplicateBindingError |
  -- | A duplicate field name error
  ErrorDuplicateField Core_.DuplicateFieldError |
  -- | Any other error
  ErrorOther OtherError |
  -- | A reference to an undefined field
  ErrorUndefinedField Core_.UndefinedFieldError |
  -- | A reference to an undefined term variable
  ErrorUndefinedTermVariable Core_.UndefinedTermVariableError |
  -- | A term variable whose type is not known
  ErrorUntypedTermVariable Core_.UntypedTermVariableError |
  -- | An unexpected term variant
  ErrorUnexpectedTermVariant Core_.UnexpectedTermVariantError |
  -- | An unexpected type variant
  ErrorUnexpectedTypeVariant Core_.UnexpectedTypeVariantError |
  -- | A type unification error
  ErrorUnification UnificationError
  deriving (Eq, Ord, Read, Show)

_Error = Core.Name "hydra.errors.Error"

_Error_checking = Core.Name "checking"

_Error_decoding = Core.Name "decoding"

_Error_duplicateBinding = Core.Name "duplicateBinding"

_Error_duplicateField = Core.Name "duplicateField"

_Error_other = Core.Name "other"

_Error_undefinedField = Core.Name "undefinedField"

_Error_undefinedTermVariable = Core.Name "undefinedTermVariable"

_Error_untypedTermVariable = Core.Name "untypedTermVariable"

_Error_unexpectedTermVariant = Core.Name "unexpectedTermVariant"

_Error_unexpectedTypeVariant = Core.Name "unexpectedTypeVariant"

_Error_unification = Core.Name "unification"

-- | Any other error
newtype OtherError =
  OtherError {
    unOtherError :: String}
  deriving (Eq, Ord, Read, Show)

_OtherError = Core.Name "hydra.errors.OtherError"

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

_UnificationError = Core.Name "hydra.errors.UnificationError"

_UnificationError_leftType = Core.Name "leftType"

_UnificationError_rightType = Core.Name "rightType"

_UnificationError_message = Core.Name "message"
