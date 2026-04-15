-- Note: this is an automatically generated file. Do not edit.

-- | Top-level error types for the Hydra kernel

module Hydra.Errors where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Paths as Paths
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | An error that occurred during decoding of a term
newtype DecodingError =
  DecodingError {
    unDecodingError :: String}
  deriving (Eq, Ord, Read, Show)

_DecodingError = Core.Name "hydra.errors.DecodingError"

-- | An empty list was encountered where a non-empty list was required
type EmptyListError = ()

_EmptyListError = Core.Name "hydra.errors.EmptyListError"

-- | An error of any kind, with kernel errors particularly differentiated
data Error =
  -- | A type checking error
  ErrorChecking Checking.CheckingError |
  -- | An error that occurred during decoding of a term
  ErrorDecoding DecodingError |
  -- | A duplicate binding name error
  ErrorDuplicateBinding ErrorCore.DuplicateBindingError |
  -- | A duplicate field name error
  ErrorDuplicateField ErrorCore.DuplicateFieldError |
  -- | An error that occurred while extracting a value from a term
  ErrorExtraction ExtractionError |
  -- | A type inference error
  ErrorInference InferenceError |
  -- | Any other error
  ErrorOther OtherError |
  -- | A name-resolution error
  ErrorResolution ResolutionError |
  -- | A reference to an undefined field
  ErrorUndefinedField ErrorCore.UndefinedFieldError |
  -- | A reference to an undefined term variable
  ErrorUndefinedTermVariable ErrorCore.UndefinedTermVariableError |
  -- | A term variable whose type is not known
  ErrorUntypedTermVariable ErrorCore.UntypedTermVariableError |
  -- | An unexpected term variant
  ErrorUnexpectedTermVariant ErrorCore.UnexpectedTermVariantError |
  -- | An unexpected type variant
  ErrorUnexpectedTypeVariant ErrorCore.UnexpectedTypeVariantError |
  -- | A type unification error
  ErrorUnification UnificationError
  deriving (Eq, Ord, Read, Show)

_Error = Core.Name "hydra.errors.Error"

_Error_checking = Core.Name "checking"

_Error_decoding = Core.Name "decoding"

_Error_duplicateBinding = Core.Name "duplicateBinding"

_Error_duplicateField = Core.Name "duplicateField"

_Error_extraction = Core.Name "extraction"

_Error_inference = Core.Name "inference"

_Error_other = Core.Name "other"

_Error_resolution = Core.Name "resolution"

_Error_undefinedField = Core.Name "undefinedField"

_Error_undefinedTermVariable = Core.Name "undefinedTermVariable"

_Error_untypedTermVariable = Core.Name "untypedTermVariable"

_Error_unexpectedTermVariant = Core.Name "unexpectedTermVariant"

_Error_unexpectedTypeVariant = Core.Name "unexpectedTypeVariant"

_Error_unification = Core.Name "unification"

-- | An error that occurred while extracting a typed value from a term
data ExtractionError =
  -- | An empty list was encountered where a non-empty list was required
  ExtractionErrorEmptyList EmptyListError |
  -- | Multiple let bindings were found with the same name
  ExtractionErrorMultipleBindings MultipleBindingsError |
  -- | Multiple record fields were found with the same field name
  ExtractionErrorMultipleFields MultipleFieldsError |
  -- | No field with the expected name was found in a record
  ExtractionErrorNoMatchingField NoMatchingFieldError |
  -- | No let binding with the expected name was found
  ExtractionErrorNoSuchBinding NoSuchBindingError |
  -- | A case statement did not contain enough cases to match the target
  ExtractionErrorNotEnoughCases NotEnoughCasesError |
  -- | A term, type, literal, or other value had an unexpected shape
  ExtractionErrorUnexpectedShape UnexpectedShapeError
  deriving (Eq, Ord, Read, Show)

_ExtractionError = Core.Name "hydra.errors.ExtractionError"

_ExtractionError_emptyList = Core.Name "emptyList"

_ExtractionError_multipleBindings = Core.Name "multipleBindings"

_ExtractionError_multipleFields = Core.Name "multipleFields"

_ExtractionError_noMatchingField = Core.Name "noMatchingField"

_ExtractionError_noSuchBinding = Core.Name "noSuchBinding"

_ExtractionError_notEnoughCases = Core.Name "notEnoughCases"

_ExtractionError_unexpectedShape = Core.Name "unexpectedShape"

-- | An error that occurred during type inference
data InferenceError =
  -- | A type checking error encountered during inference
  InferenceErrorChecking Checking.CheckingError |
  -- | A generic inference error carrying a message and a subterm path. Placeholder arm; sites should migrate to typed variants.
  InferenceErrorOther OtherInferenceError |
  -- | A unification failure encountered while inferring types
  InferenceErrorUnification UnificationInferenceError
  deriving (Eq, Ord, Read, Show)

_InferenceError = Core.Name "hydra.errors.InferenceError"

_InferenceError_checking = Core.Name "checking"

_InferenceError_other = Core.Name "other"

_InferenceError_unification = Core.Name "unification"

-- | Multiple let bindings with the same name were found
data MultipleBindingsError =
  MultipleBindingsError {
    -- | The binding name which was duplicated
    multipleBindingsErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_MultipleBindingsError = Core.Name "hydra.errors.MultipleBindingsError"

_MultipleBindingsError_name = Core.Name "name"

-- | Multiple fields with the same name were found in a record
data MultipleFieldsError =
  MultipleFieldsError {
    -- | The field name which appeared more than once
    multipleFieldsErrorFieldName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_MultipleFieldsError = Core.Name "hydra.errors.MultipleFieldsError"

_MultipleFieldsError_fieldName = Core.Name "fieldName"

-- | No field with the expected name was present
data NoMatchingFieldError =
  NoMatchingFieldError {
    -- | The field name which was not found
    noMatchingFieldErrorFieldName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_NoMatchingFieldError = Core.Name "hydra.errors.NoMatchingFieldError"

_NoMatchingFieldError_fieldName = Core.Name "fieldName"

-- | No let binding with the expected name was present
data NoSuchBindingError =
  NoSuchBindingError {
    -- | The binding name which was not found
    noSuchBindingErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_NoSuchBindingError = Core.Name "hydra.errors.NoSuchBindingError"

_NoSuchBindingError_name = Core.Name "name"

-- | No primitive function with the expected name was registered in the graph
data NoSuchPrimitiveError =
  NoSuchPrimitiveError {
    -- | The primitive name which was not found
    noSuchPrimitiveErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_NoSuchPrimitiveError = Core.Name "hydra.errors.NoSuchPrimitiveError"

_NoSuchPrimitiveError_name = Core.Name "name"

-- | A case statement was missing a case for the requested variant
type NotEnoughCasesError = ()

_NotEnoughCasesError = Core.Name "hydra.errors.NotEnoughCasesError"

-- | Any other error
newtype OtherError =
  OtherError {
    unOtherError :: String}
  deriving (Eq, Ord, Read, Show)

_OtherError = Core.Name "hydra.errors.OtherError"

-- | A generic inference error: message + subterm path
data OtherInferenceError =
  OtherInferenceError {
    -- | The subterm path at which the error was observed
    otherInferenceErrorPath :: Paths.SubtermPath,
    -- | A human-readable error message
    otherInferenceErrorMessage :: String}
  deriving (Eq, Ord, Read, Show)

_OtherInferenceError = Core.Name "hydra.errors.OtherInferenceError"

_OtherInferenceError_path = Core.Name "path"

_OtherInferenceError_message = Core.Name "message"

-- | A generic resolution error: message
newtype OtherResolutionError =
  OtherResolutionError {
    unOtherResolutionError :: String}
  deriving (Eq, Ord, Read, Show)

_OtherResolutionError = Core.Name "hydra.errors.OtherResolutionError"

-- | An error that occurred while resolving a name, primitive, or record/union shape in a graph
data ResolutionError =
  -- | No binding with the expected name was found in the graph
  ResolutionErrorNoSuchBinding NoSuchBindingError |
  -- | No primitive function with the expected name was found in the graph
  ResolutionErrorNoSuchPrimitive NoSuchPrimitiveError |
  -- | No field with the expected name was present in a record or case statement
  ResolutionErrorNoMatchingField NoMatchingFieldError |
  -- | A generic resolution error carrying a message
  ResolutionErrorOther OtherResolutionError |
  -- | A term had a shape other than the one expected (e.g. a record, an injection)
  ResolutionErrorUnexpectedShape UnexpectedShapeError
  deriving (Eq, Ord, Read, Show)

_ResolutionError = Core.Name "hydra.errors.ResolutionError"

_ResolutionError_noSuchBinding = Core.Name "noSuchBinding"

_ResolutionError_noSuchPrimitive = Core.Name "noSuchPrimitive"

_ResolutionError_noMatchingField = Core.Name "noMatchingField"

_ResolutionError_other = Core.Name "other"

_ResolutionError_unexpectedShape = Core.Name "unexpectedShape"

-- | A term, type, literal, or related value had a shape other than the one expected
data UnexpectedShapeError =
  UnexpectedShapeError {
    -- | A description of the expected shape
    unexpectedShapeErrorExpected :: String,
    -- | A description of the shape actually encountered
    unexpectedShapeErrorActual :: String}
  deriving (Eq, Ord, Read, Show)

_UnexpectedShapeError = Core.Name "hydra.errors.UnexpectedShapeError"

_UnexpectedShapeError_expected = Core.Name "expected"

_UnexpectedShapeError_actual = Core.Name "actual"

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

-- | A unification failure at a specific subterm locus during inference
data UnificationInferenceError =
  UnificationInferenceError {
    -- | The subterm path at which the unification failure was observed
    unificationInferenceErrorPath :: Paths.SubtermPath,
    -- | The underlying unification error
    unificationInferenceErrorCause :: UnificationError}
  deriving (Eq, Ord, Read, Show)

_UnificationInferenceError = Core.Name "hydra.errors.UnificationInferenceError"

_UnificationInferenceError_path = Core.Name "path"

_UnificationInferenceError_cause = Core.Name "cause"
