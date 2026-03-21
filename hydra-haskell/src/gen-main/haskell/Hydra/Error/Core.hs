-- Note: this is an automatically generated file. Do not edit.

-- | Error types for core type and term validation

module Hydra.Error.Core where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Core as Core
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A duplicate binding name in a let expression
data DuplicateBindingError =
  DuplicateBindingError {
    -- | The path to the duplicate binding within the term
    duplicateBindingErrorLocation :: Accessors.AccessorPath,
    -- | The duplicated binding name
    duplicateBindingErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_DuplicateBindingError = Core.Name "hydra.error.core.DuplicateBindingError"

_DuplicateBindingError_location = Core.Name "location"

_DuplicateBindingError_name = Core.Name "name"

-- | A duplicate field name in a record or union type
data DuplicateFieldError =
  DuplicateFieldError {
    -- | The path to the duplicate field within the term
    duplicateFieldErrorLocation :: Accessors.AccessorPath,
    -- | The duplicated field name
    duplicateFieldErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_DuplicateFieldError = Core.Name "hydra.error.core.DuplicateFieldError"

_DuplicateFieldError_location = Core.Name "location"

_DuplicateFieldError_name = Core.Name "name"

-- | An error indicating that a term is invalid
data InvalidTermError =
  -- | A duplicate binding name in a let expression
  InvalidTermErrorDuplicateBinding DuplicateBindingError |
  -- | A duplicate field name in a record or union type
  InvalidTermErrorDuplicateField DuplicateFieldError
  deriving (Eq, Ord, Read, Show)

_InvalidTermError = Core.Name "hydra.error.core.InvalidTermError"

_InvalidTermError_duplicateBinding = Core.Name "duplicateBinding"

_InvalidTermError_duplicateField = Core.Name "duplicateField"

-- | A reference to a field that does not exist in the given type
data UndefinedFieldError =
  UndefinedFieldError {
    -- | The name of the undefined field
    undefinedFieldErrorFieldName :: Core.Name,
    -- | The name of the type in which the field was expected
    undefinedFieldErrorTypeName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedFieldError = Core.Name "hydra.error.core.UndefinedFieldError"

_UndefinedFieldError_fieldName = Core.Name "fieldName"

_UndefinedFieldError_typeName = Core.Name "typeName"

-- | A reference to a term (element, binding, or primitive) that is not defined
data UndefinedTermError =
  UndefinedTermError {
    -- | The name of the undefined term
    undefinedTermErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedTermError = Core.Name "hydra.error.core.UndefinedTermError"

_UndefinedTermError_name = Core.Name "name"

-- | A reference to a type or type variable that is not defined
data UndefinedTypeError =
  UndefinedTypeError {
    -- | The name of the undefined type
    undefinedTypeErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedTypeError = Core.Name "hydra.error.core.UndefinedTypeError"

_UndefinedTypeError_name = Core.Name "name"

-- | An unexpected term variant was encountered
data UnexpectedTermVariantError =
  UnexpectedTermVariantError {
    -- | The expected term variant
    unexpectedTermVariantErrorExpectedVariant :: Variants.TermVariant,
    -- | The actual term that was encountered
    unexpectedTermVariantErrorActualTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_UnexpectedTermVariantError = Core.Name "hydra.error.core.UnexpectedTermVariantError"

_UnexpectedTermVariantError_expectedVariant = Core.Name "expectedVariant"

_UnexpectedTermVariantError_actualTerm = Core.Name "actualTerm"

-- | An unexpected type variant was encountered
data UnexpectedTypeVariantError =
  UnexpectedTypeVariantError {
    -- | The expected type variant
    unexpectedTypeVariantErrorExpectedVariant :: Variants.TypeVariant,
    -- | The actual type that was encountered
    unexpectedTypeVariantErrorActualType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_UnexpectedTypeVariantError = Core.Name "hydra.error.core.UnexpectedTypeVariantError"

_UnexpectedTypeVariantError_expectedVariant = Core.Name "expectedVariant"

_UnexpectedTypeVariantError_actualType = Core.Name "actualType"
