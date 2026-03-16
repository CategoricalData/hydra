-- Note: this is an automatically generated file. Do not edit.

-- | Error types specific to the Hydra kernel

module Hydra.Error where

import qualified Hydra.Core as Core
import qualified Hydra.Typing as Typing
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An error that occurred during type checking
data CheckingError = 
  -- | A post-unification consistency check failure
  CheckingErrorIncorrectUnification IncorrectUnificationError |
  -- | A type that is not a forall type when one was expected
  CheckingErrorNotAForallType NotAForallTypeError |
  -- | A type that is not a function type when one was expected
  CheckingErrorNotAFunctionType NotAFunctionTypeError |
  -- | A type constructor applied to the wrong number of arguments
  CheckingErrorTypeArityMismatch TypeArityMismatchError |
  -- | A type mismatch between expected and actual types
  CheckingErrorTypeMismatch TypeMismatchError |
  -- | Type variables that are not bound in scope
  CheckingErrorUnboundTypeVariables UnboundTypeVariablesError |
  -- | Multiple types that should be equal but are not
  CheckingErrorUnequalTypes UnequalTypesError |
  -- | A term variant that the type checker does not support
  CheckingErrorUnsupportedTermVariant UnsupportedTermVariantError |
  -- | A lambda expression without a type annotation on its parameter
  CheckingErrorUntypedLambda UntypedLambdaError |
  -- | A let binding without a type annotation
  CheckingErrorUntypedLetBinding UntypedLetBindingError
  deriving (Eq, Ord, Read, Show)

_CheckingError = Core.Name "hydra.error.CheckingError"

_CheckingError_incorrectUnification = Core.Name "incorrectUnification"

_CheckingError_notAForallType = Core.Name "notAForallType"

_CheckingError_notAFunctionType = Core.Name "notAFunctionType"

_CheckingError_typeArityMismatch = Core.Name "typeArityMismatch"

_CheckingError_typeMismatch = Core.Name "typeMismatch"

_CheckingError_unboundTypeVariables = Core.Name "unboundTypeVariables"

_CheckingError_unequalTypes = Core.Name "unequalTypes"

_CheckingError_unsupportedTermVariant = Core.Name "unsupportedTermVariant"

_CheckingError_untypedLambda = Core.Name "untypedLambda"

_CheckingError_untypedLetBinding = Core.Name "untypedLetBinding"

-- | An error that occurred during decoding of a term
newtype DecodingError = 
  DecodingError {
    unDecodingError :: String}
  deriving (Eq, Ord, Read, Show)

_DecodingError = Core.Name "hydra.error.DecodingError"

-- | A duplicate binding name in a let expression
data DuplicateBindingError = 
  DuplicateBindingError {
    -- | The duplicated binding name
    duplicateBindingErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_DuplicateBindingError = Core.Name "hydra.error.DuplicateBindingError"

_DuplicateBindingError_name = Core.Name "name"

-- | A duplicate field name in a record or union type
data DuplicateFieldError = 
  DuplicateFieldError {
    -- | The duplicated field name
    duplicateFieldErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_DuplicateFieldError = Core.Name "hydra.error.DuplicateFieldError"

_DuplicateFieldError_name = Core.Name "name"

-- | An error of any kind, with kernel errors particularly differentiated
data Error = 
  -- | A type checking error
  ErrorChecking CheckingError |
  -- | An error that occurred during decoding of a term
  ErrorDecoding DecodingError |
  -- | A duplicate binding name error
  ErrorDuplicateBinding DuplicateBindingError |
  -- | A duplicate field name error
  ErrorDuplicateField DuplicateFieldError |
  -- | Any other error
  ErrorOther OtherError |
  -- | A reference to an undefined field
  ErrorUndefinedField UndefinedFieldError |
  -- | A reference to an undefined term
  ErrorUndefinedTerm UndefinedTermError |
  -- | A reference to an undefined type
  ErrorUndefinedType UndefinedTypeError |
  -- | An unexpected term variant
  ErrorUnexpectedTermVariant UnexpectedTermVariantError |
  -- | An unexpected type variant
  ErrorUnexpectedTypeVariant UnexpectedTypeVariantError |
  -- | A type unification error
  ErrorUnification UnificationError
  deriving (Eq, Ord, Read, Show)

_Error = Core.Name "hydra.error.Error"

_Error_checking = Core.Name "checking"

_Error_decoding = Core.Name "decoding"

_Error_duplicateBinding = Core.Name "duplicateBinding"

_Error_duplicateField = Core.Name "duplicateField"

_Error_other = Core.Name "other"

_Error_undefinedField = Core.Name "undefinedField"

_Error_undefinedTerm = Core.Name "undefinedTerm"

_Error_undefinedType = Core.Name "undefinedType"

_Error_unexpectedTermVariant = Core.Name "unexpectedTermVariant"

_Error_unexpectedTypeVariant = Core.Name "unexpectedTypeVariant"

_Error_unification = Core.Name "unification"

-- | A post-unification consistency check failure
data IncorrectUnificationError = 
  IncorrectUnificationError {
    -- | The substitution that failed the consistency check
    incorrectUnificationErrorSubstitution :: Typing.TypeSubst}
  deriving (Eq, Ord, Read, Show)

_IncorrectUnificationError = Core.Name "hydra.error.IncorrectUnificationError"

_IncorrectUnificationError_substitution = Core.Name "substitution"

-- | A type that is not a forall type when type arguments are being applied
data NotAForallTypeError = 
  NotAForallTypeError {
    -- | The actual type encountered
    notAForallTypeErrorType :: Core.Type,
    -- | The type arguments that were being applied
    notAForallTypeErrorTypeArguments :: [Core.Type]}
  deriving (Eq, Ord, Read, Show)

_NotAForallTypeError = Core.Name "hydra.error.NotAForallTypeError"

_NotAForallTypeError_type = Core.Name "type"

_NotAForallTypeError_typeArguments = Core.Name "typeArguments"

-- | A type that is not a function type when one was expected in an application
data NotAFunctionTypeError = 
  NotAFunctionTypeError {
    -- | The actual type encountered
    notAFunctionTypeErrorType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_NotAFunctionTypeError = Core.Name "hydra.error.NotAFunctionTypeError"

_NotAFunctionTypeError_type = Core.Name "type"

-- | Any other error
newtype OtherError = 
  OtherError {
    unOtherError :: String}
  deriving (Eq, Ord, Read, Show)

_OtherError = Core.Name "hydra.error.OtherError"

-- | A type constructor applied to the wrong number of type arguments
data TypeArityMismatchError = 
  TypeArityMismatchError {
    -- | The type being checked
    typeArityMismatchErrorType :: Core.Type,
    -- | The expected number of type arguments
    typeArityMismatchErrorExpectedArity :: Int,
    -- | The actual number of type arguments provided
    typeArityMismatchErrorActualArity :: Int,
    -- | The type arguments that were provided
    typeArityMismatchErrorTypeArguments :: [Core.Type]}
  deriving (Eq, Ord, Read, Show)

_TypeArityMismatchError = Core.Name "hydra.error.TypeArityMismatchError"

_TypeArityMismatchError_type = Core.Name "type"

_TypeArityMismatchError_expectedArity = Core.Name "expectedArity"

_TypeArityMismatchError_actualArity = Core.Name "actualArity"

_TypeArityMismatchError_typeArguments = Core.Name "typeArguments"

-- | A type mismatch between expected and actual types
data TypeMismatchError = 
  TypeMismatchError {
    -- | The expected type
    typeMismatchErrorExpectedType :: Core.Type,
    -- | The actual type encountered
    typeMismatchErrorActualType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_TypeMismatchError = Core.Name "hydra.error.TypeMismatchError"

_TypeMismatchError_expectedType = Core.Name "expectedType"

_TypeMismatchError_actualType = Core.Name "actualType"

-- | Type variables that appear free in a type but are not bound in scope
data UnboundTypeVariablesError = 
  UnboundTypeVariablesError {
    -- | The set of unbound type variable names
    unboundTypeVariablesErrorVariables :: (S.Set Core.Name),
    -- | The type containing the unbound variables
    unboundTypeVariablesErrorType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_UnboundTypeVariablesError = Core.Name "hydra.error.UnboundTypeVariablesError"

_UnboundTypeVariablesError_variables = Core.Name "variables"

_UnboundTypeVariablesError_type = Core.Name "type"

-- | A reference to a field that does not exist in the given type
data UndefinedFieldError = 
  UndefinedFieldError {
    -- | The name of the undefined field
    undefinedFieldErrorFieldName :: Core.Name,
    -- | The name of the type in which the field was expected
    undefinedFieldErrorTypeName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedFieldError = Core.Name "hydra.error.UndefinedFieldError"

_UndefinedFieldError_fieldName = Core.Name "fieldName"

_UndefinedFieldError_typeName = Core.Name "typeName"

-- | A reference to a term (element, binding, or primitive) that is not defined
data UndefinedTermError = 
  UndefinedTermError {
    -- | The name of the undefined term
    undefinedTermErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedTermError = Core.Name "hydra.error.UndefinedTermError"

_UndefinedTermError_name = Core.Name "name"

-- | A reference to a type or type variable that is not defined
data UndefinedTypeError = 
  UndefinedTypeError {
    -- | The name of the undefined type
    undefinedTypeErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_UndefinedTypeError = Core.Name "hydra.error.UndefinedTypeError"

_UndefinedTypeError_name = Core.Name "name"

-- | Multiple types that should all be equal but are not
data UnequalTypesError = 
  UnequalTypesError {
    -- | The list of types that are not all equal
    unequalTypesErrorTypes :: [Core.Type],
    -- | A description of the context in which the types were expected to be equal
    unequalTypesErrorDescription :: String}
  deriving (Eq, Ord, Read, Show)

_UnequalTypesError = Core.Name "hydra.error.UnequalTypesError"

_UnequalTypesError_types = Core.Name "types"

_UnequalTypesError_description = Core.Name "description"

-- | An unexpected term variant was encountered
data UnexpectedTermVariantError = 
  UnexpectedTermVariantError {
    -- | The expected term variant
    unexpectedTermVariantErrorExpectedVariant :: Variants.TermVariant,
    -- | The actual term that was encountered
    unexpectedTermVariantErrorActualTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_UnexpectedTermVariantError = Core.Name "hydra.error.UnexpectedTermVariantError"

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

_UnexpectedTypeVariantError = Core.Name "hydra.error.UnexpectedTypeVariantError"

_UnexpectedTypeVariantError_expectedVariant = Core.Name "expectedVariant"

_UnexpectedTypeVariantError_actualType = Core.Name "actualType"

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

_UnificationError = Core.Name "hydra.error.UnificationError"

_UnificationError_leftType = Core.Name "leftType"

_UnificationError_rightType = Core.Name "rightType"

_UnificationError_message = Core.Name "message"

-- | A term variant that the type checker does not support
data UnsupportedTermVariantError = 
  UnsupportedTermVariantError {
    -- | The unsupported term variant
    unsupportedTermVariantErrorTermVariant :: Variants.TermVariant}
  deriving (Eq, Ord, Read, Show)

_UnsupportedTermVariantError = Core.Name "hydra.error.UnsupportedTermVariantError"

_UnsupportedTermVariantError_termVariant = Core.Name "termVariant"

-- | A lambda expression without a type annotation on its parameter
data UntypedLambdaError = 
  UntypedLambdaError {}
  deriving (Eq, Ord, Read, Show)

_UntypedLambdaError = Core.Name "hydra.error.UntypedLambdaError"

-- | A let binding without a type annotation
data UntypedLetBindingError = 
  UntypedLetBindingError {
    -- | The untyped binding
    untypedLetBindingErrorBinding :: Core.Binding}
  deriving (Eq, Ord, Read, Show)

_UntypedLetBindingError = Core.Name "hydra.error.UntypedLetBindingError"

_UntypedLetBindingError_binding = Core.Name "binding"
