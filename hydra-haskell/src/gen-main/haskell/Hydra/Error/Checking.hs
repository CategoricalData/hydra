-- Note: this is an automatically generated file. Do not edit.

-- | Error types for type checking

module Hydra.Error.Checking where

import qualified Hydra.Core as Core
import qualified Hydra.Typing as Typing
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
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

_CheckingError = Core.Name "hydra.error.checking.CheckingError"

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

-- | A post-unification consistency check failure
data IncorrectUnificationError =
  IncorrectUnificationError {
    -- | The substitution that failed the consistency check
    incorrectUnificationErrorSubstitution :: Typing.TypeSubst}
  deriving (Eq, Ord, Read, Show)

_IncorrectUnificationError = Core.Name "hydra.error.checking.IncorrectUnificationError"

_IncorrectUnificationError_substitution = Core.Name "substitution"

-- | A type that is not a forall type when type arguments are being applied
data NotAForallTypeError =
  NotAForallTypeError {
    -- | The actual type encountered
    notAForallTypeErrorType :: Core.Type,
    -- | The type arguments that were being applied
    notAForallTypeErrorTypeArguments :: [Core.Type]}
  deriving (Eq, Ord, Read, Show)

_NotAForallTypeError = Core.Name "hydra.error.checking.NotAForallTypeError"

_NotAForallTypeError_type = Core.Name "type"

_NotAForallTypeError_typeArguments = Core.Name "typeArguments"

-- | A type that is not a function type when one was expected in an application
data NotAFunctionTypeError =
  NotAFunctionTypeError {
    -- | The actual type encountered
    notAFunctionTypeErrorType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_NotAFunctionTypeError = Core.Name "hydra.error.checking.NotAFunctionTypeError"

_NotAFunctionTypeError_type = Core.Name "type"

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

_TypeArityMismatchError = Core.Name "hydra.error.checking.TypeArityMismatchError"

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

_TypeMismatchError = Core.Name "hydra.error.checking.TypeMismatchError"

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

_UnboundTypeVariablesError = Core.Name "hydra.error.checking.UnboundTypeVariablesError"

_UnboundTypeVariablesError_variables = Core.Name "variables"

_UnboundTypeVariablesError_type = Core.Name "type"

-- | Multiple types that should all be equal but are not
data UnequalTypesError =
  UnequalTypesError {
    -- | The list of types that are not all equal
    unequalTypesErrorTypes :: [Core.Type],
    -- | A description of the context in which the types were expected to be equal
    unequalTypesErrorDescription :: String}
  deriving (Eq, Ord, Read, Show)

_UnequalTypesError = Core.Name "hydra.error.checking.UnequalTypesError"

_UnequalTypesError_types = Core.Name "types"

_UnequalTypesError_description = Core.Name "description"

-- | A term variant that the type checker does not support
data UnsupportedTermVariantError =
  UnsupportedTermVariantError {
    -- | The unsupported term variant
    unsupportedTermVariantErrorTermVariant :: Variants.TermVariant}
  deriving (Eq, Ord, Read, Show)

_UnsupportedTermVariantError = Core.Name "hydra.error.checking.UnsupportedTermVariantError"

_UnsupportedTermVariantError_termVariant = Core.Name "termVariant"

-- | A lambda expression without a type annotation on its parameter
data UntypedLambdaError =
  UntypedLambdaError {}
  deriving (Eq, Ord, Read, Show)

_UntypedLambdaError = Core.Name "hydra.error.checking.UntypedLambdaError"

-- | A let binding without a type annotation
data UntypedLetBindingError =
  UntypedLetBindingError {
    -- | The untyped binding
    untypedLetBindingErrorBinding :: Core.Binding}
  deriving (Eq, Ord, Read, Show)

_UntypedLetBindingError = Core.Name "hydra.error.checking.UntypedLetBindingError"

_UntypedLetBindingError_binding = Core.Name "binding"
