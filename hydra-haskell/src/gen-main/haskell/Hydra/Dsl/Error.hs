-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.error

module Hydra.Dsl.Error where

import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Typing as Typing
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

checkingErrorIncorrectUnification :: (Error.IncorrectUnificationError -> Error.CheckingError)
checkingErrorIncorrectUnification x = (Error.CheckingErrorIncorrectUnification x)

checkingErrorNotAForallType :: (Error.NotAForallTypeError -> Error.CheckingError)
checkingErrorNotAForallType x = (Error.CheckingErrorNotAForallType x)

checkingErrorNotAFunctionType :: (Error.NotAFunctionTypeError -> Error.CheckingError)
checkingErrorNotAFunctionType x = (Error.CheckingErrorNotAFunctionType x)

checkingErrorTypeArityMismatch :: (Error.TypeArityMismatchError -> Error.CheckingError)
checkingErrorTypeArityMismatch x = (Error.CheckingErrorTypeArityMismatch x)

checkingErrorTypeMismatch :: (Error.TypeMismatchError -> Error.CheckingError)
checkingErrorTypeMismatch x = (Error.CheckingErrorTypeMismatch x)

checkingErrorUnboundTypeVariables :: (Error.UnboundTypeVariablesError -> Error.CheckingError)
checkingErrorUnboundTypeVariables x = (Error.CheckingErrorUnboundTypeVariables x)

checkingErrorUnequalTypes :: (Error.UnequalTypesError -> Error.CheckingError)
checkingErrorUnequalTypes x = (Error.CheckingErrorUnequalTypes x)

checkingErrorUnsupportedTermVariant :: (Error.UnsupportedTermVariantError -> Error.CheckingError)
checkingErrorUnsupportedTermVariant x = (Error.CheckingErrorUnsupportedTermVariant x)

checkingErrorUntypedLambda :: (Error.UntypedLambdaError -> Error.CheckingError)
checkingErrorUntypedLambda x = (Error.CheckingErrorUntypedLambda x)

checkingErrorUntypedLetBinding :: (Error.UntypedLetBindingError -> Error.CheckingError)
checkingErrorUntypedLetBinding x = (Error.CheckingErrorUntypedLetBinding x)

decodingError :: (String -> Error.DecodingError)
decodingError x = (Error.DecodingError x)

unDecodingError :: (Error.DecodingError -> String)
unDecodingError = Error.unDecodingError

duplicateBindingError :: (Core.Name -> Error.DuplicateBindingError)
duplicateBindingError name = Error.DuplicateBindingError {
  Error.duplicateBindingErrorName = name}

duplicateBindingErrorName :: (Error.DuplicateBindingError -> Core.Name)
duplicateBindingErrorName = Error.duplicateBindingErrorName

duplicateBindingErrorWithName :: (t0 -> Core.Name -> Error.DuplicateBindingError)
duplicateBindingErrorWithName original newVal = Error.DuplicateBindingError {
  Error.duplicateBindingErrorName = newVal}

duplicateFieldError :: (Core.Name -> Error.DuplicateFieldError)
duplicateFieldError name = Error.DuplicateFieldError {
  Error.duplicateFieldErrorName = name}

duplicateFieldErrorName :: (Error.DuplicateFieldError -> Core.Name)
duplicateFieldErrorName = Error.duplicateFieldErrorName

duplicateFieldErrorWithName :: (t0 -> Core.Name -> Error.DuplicateFieldError)
duplicateFieldErrorWithName original newVal = Error.DuplicateFieldError {
  Error.duplicateFieldErrorName = newVal}

errorChecking :: (Error.CheckingError -> Error.Error)
errorChecking x = (Error.ErrorChecking x)

errorDecoding :: (Error.DecodingError -> Error.Error)
errorDecoding x = (Error.ErrorDecoding x)

errorDuplicateBinding :: (Error.DuplicateBindingError -> Error.Error)
errorDuplicateBinding x = (Error.ErrorDuplicateBinding x)

errorDuplicateField :: (Error.DuplicateFieldError -> Error.Error)
errorDuplicateField x = (Error.ErrorDuplicateField x)

errorOther :: (Error.OtherError -> Error.Error)
errorOther x = (Error.ErrorOther x)

errorUndefinedField :: (Error.UndefinedFieldError -> Error.Error)
errorUndefinedField x = (Error.ErrorUndefinedField x)

errorUndefinedTerm :: (Error.UndefinedTermError -> Error.Error)
errorUndefinedTerm x = (Error.ErrorUndefinedTerm x)

errorUndefinedType :: (Error.UndefinedTypeError -> Error.Error)
errorUndefinedType x = (Error.ErrorUndefinedType x)

errorUnexpectedTermVariant :: (Error.UnexpectedTermVariantError -> Error.Error)
errorUnexpectedTermVariant x = (Error.ErrorUnexpectedTermVariant x)

errorUnexpectedTypeVariant :: (Error.UnexpectedTypeVariantError -> Error.Error)
errorUnexpectedTypeVariant x = (Error.ErrorUnexpectedTypeVariant x)

errorUnification :: (Error.UnificationError -> Error.Error)
errorUnification x = (Error.ErrorUnification x)

incorrectUnificationError :: (Typing.TypeSubst -> Error.IncorrectUnificationError)
incorrectUnificationError substitution = Error.IncorrectUnificationError {
  Error.incorrectUnificationErrorSubstitution = substitution}

incorrectUnificationErrorSubstitution :: (Error.IncorrectUnificationError -> Typing.TypeSubst)
incorrectUnificationErrorSubstitution = Error.incorrectUnificationErrorSubstitution

incorrectUnificationErrorWithSubstitution :: (t0 -> Typing.TypeSubst -> Error.IncorrectUnificationError)
incorrectUnificationErrorWithSubstitution original newVal = Error.IncorrectUnificationError {
  Error.incorrectUnificationErrorSubstitution = newVal}

notAForallTypeError :: (Core.Type -> [Core.Type] -> Error.NotAForallTypeError)
notAForallTypeError type_ typeArguments = Error.NotAForallTypeError {
  Error.notAForallTypeErrorType = type_,
  Error.notAForallTypeErrorTypeArguments = typeArguments}

notAForallTypeErrorType :: (Error.NotAForallTypeError -> Core.Type)
notAForallTypeErrorType = Error.notAForallTypeErrorType

notAForallTypeErrorTypeArguments :: (Error.NotAForallTypeError -> [Core.Type])
notAForallTypeErrorTypeArguments = Error.notAForallTypeErrorTypeArguments

notAForallTypeErrorWithType :: (Error.NotAForallTypeError -> Core.Type -> Error.NotAForallTypeError)
notAForallTypeErrorWithType original newVal = Error.NotAForallTypeError {
  Error.notAForallTypeErrorType = newVal,
  Error.notAForallTypeErrorTypeArguments = (Error.notAForallTypeErrorTypeArguments original)}

notAForallTypeErrorWithTypeArguments :: (Error.NotAForallTypeError -> [Core.Type] -> Error.NotAForallTypeError)
notAForallTypeErrorWithTypeArguments original newVal = Error.NotAForallTypeError {
  Error.notAForallTypeErrorType = (Error.notAForallTypeErrorType original),
  Error.notAForallTypeErrorTypeArguments = newVal}

notAFunctionTypeError :: (Core.Type -> Error.NotAFunctionTypeError)
notAFunctionTypeError type_ = Error.NotAFunctionTypeError {
  Error.notAFunctionTypeErrorType = type_}

notAFunctionTypeErrorType :: (Error.NotAFunctionTypeError -> Core.Type)
notAFunctionTypeErrorType = Error.notAFunctionTypeErrorType

notAFunctionTypeErrorWithType :: (t0 -> Core.Type -> Error.NotAFunctionTypeError)
notAFunctionTypeErrorWithType original newVal = Error.NotAFunctionTypeError {
  Error.notAFunctionTypeErrorType = newVal}

otherError :: (String -> Error.OtherError)
otherError x = (Error.OtherError x)

unOtherError :: (Error.OtherError -> String)
unOtherError = Error.unOtherError

typeArityMismatchError :: (Core.Type -> Int -> Int -> [Core.Type] -> Error.TypeArityMismatchError)
typeArityMismatchError type_ expectedArity actualArity typeArguments = Error.TypeArityMismatchError {
  Error.typeArityMismatchErrorType = type_,
  Error.typeArityMismatchErrorExpectedArity = expectedArity,
  Error.typeArityMismatchErrorActualArity = actualArity,
  Error.typeArityMismatchErrorTypeArguments = typeArguments}

typeArityMismatchErrorType :: (Error.TypeArityMismatchError -> Core.Type)
typeArityMismatchErrorType = Error.typeArityMismatchErrorType

typeArityMismatchErrorExpectedArity :: (Error.TypeArityMismatchError -> Int)
typeArityMismatchErrorExpectedArity = Error.typeArityMismatchErrorExpectedArity

typeArityMismatchErrorActualArity :: (Error.TypeArityMismatchError -> Int)
typeArityMismatchErrorActualArity = Error.typeArityMismatchErrorActualArity

typeArityMismatchErrorTypeArguments :: (Error.TypeArityMismatchError -> [Core.Type])
typeArityMismatchErrorTypeArguments = Error.typeArityMismatchErrorTypeArguments

typeArityMismatchErrorWithType :: (Error.TypeArityMismatchError -> Core.Type -> Error.TypeArityMismatchError)
typeArityMismatchErrorWithType original newVal = Error.TypeArityMismatchError {
  Error.typeArityMismatchErrorType = newVal,
  Error.typeArityMismatchErrorExpectedArity = (Error.typeArityMismatchErrorExpectedArity original),
  Error.typeArityMismatchErrorActualArity = (Error.typeArityMismatchErrorActualArity original),
  Error.typeArityMismatchErrorTypeArguments = (Error.typeArityMismatchErrorTypeArguments original)}

typeArityMismatchErrorWithExpectedArity :: (Error.TypeArityMismatchError -> Int -> Error.TypeArityMismatchError)
typeArityMismatchErrorWithExpectedArity original newVal = Error.TypeArityMismatchError {
  Error.typeArityMismatchErrorType = (Error.typeArityMismatchErrorType original),
  Error.typeArityMismatchErrorExpectedArity = newVal,
  Error.typeArityMismatchErrorActualArity = (Error.typeArityMismatchErrorActualArity original),
  Error.typeArityMismatchErrorTypeArguments = (Error.typeArityMismatchErrorTypeArguments original)}

typeArityMismatchErrorWithActualArity :: (Error.TypeArityMismatchError -> Int -> Error.TypeArityMismatchError)
typeArityMismatchErrorWithActualArity original newVal = Error.TypeArityMismatchError {
  Error.typeArityMismatchErrorType = (Error.typeArityMismatchErrorType original),
  Error.typeArityMismatchErrorExpectedArity = (Error.typeArityMismatchErrorExpectedArity original),
  Error.typeArityMismatchErrorActualArity = newVal,
  Error.typeArityMismatchErrorTypeArguments = (Error.typeArityMismatchErrorTypeArguments original)}

typeArityMismatchErrorWithTypeArguments :: (Error.TypeArityMismatchError -> [Core.Type] -> Error.TypeArityMismatchError)
typeArityMismatchErrorWithTypeArguments original newVal = Error.TypeArityMismatchError {
  Error.typeArityMismatchErrorType = (Error.typeArityMismatchErrorType original),
  Error.typeArityMismatchErrorExpectedArity = (Error.typeArityMismatchErrorExpectedArity original),
  Error.typeArityMismatchErrorActualArity = (Error.typeArityMismatchErrorActualArity original),
  Error.typeArityMismatchErrorTypeArguments = newVal}

typeMismatchError :: (Core.Type -> Core.Type -> Error.TypeMismatchError)
typeMismatchError expectedType actualType = Error.TypeMismatchError {
  Error.typeMismatchErrorExpectedType = expectedType,
  Error.typeMismatchErrorActualType = actualType}

typeMismatchErrorExpectedType :: (Error.TypeMismatchError -> Core.Type)
typeMismatchErrorExpectedType = Error.typeMismatchErrorExpectedType

typeMismatchErrorActualType :: (Error.TypeMismatchError -> Core.Type)
typeMismatchErrorActualType = Error.typeMismatchErrorActualType

typeMismatchErrorWithExpectedType :: (Error.TypeMismatchError -> Core.Type -> Error.TypeMismatchError)
typeMismatchErrorWithExpectedType original newVal = Error.TypeMismatchError {
  Error.typeMismatchErrorExpectedType = newVal,
  Error.typeMismatchErrorActualType = (Error.typeMismatchErrorActualType original)}

typeMismatchErrorWithActualType :: (Error.TypeMismatchError -> Core.Type -> Error.TypeMismatchError)
typeMismatchErrorWithActualType original newVal = Error.TypeMismatchError {
  Error.typeMismatchErrorExpectedType = (Error.typeMismatchErrorExpectedType original),
  Error.typeMismatchErrorActualType = newVal}

unboundTypeVariablesError :: (S.Set Core.Name -> Core.Type -> Error.UnboundTypeVariablesError)
unboundTypeVariablesError variables type_ = Error.UnboundTypeVariablesError {
  Error.unboundTypeVariablesErrorVariables = variables,
  Error.unboundTypeVariablesErrorType = type_}

unboundTypeVariablesErrorVariables :: (Error.UnboundTypeVariablesError -> S.Set Core.Name)
unboundTypeVariablesErrorVariables = Error.unboundTypeVariablesErrorVariables

unboundTypeVariablesErrorType :: (Error.UnboundTypeVariablesError -> Core.Type)
unboundTypeVariablesErrorType = Error.unboundTypeVariablesErrorType

unboundTypeVariablesErrorWithVariables :: (Error.UnboundTypeVariablesError -> S.Set Core.Name -> Error.UnboundTypeVariablesError)
unboundTypeVariablesErrorWithVariables original newVal = Error.UnboundTypeVariablesError {
  Error.unboundTypeVariablesErrorVariables = newVal,
  Error.unboundTypeVariablesErrorType = (Error.unboundTypeVariablesErrorType original)}

unboundTypeVariablesErrorWithType :: (Error.UnboundTypeVariablesError -> Core.Type -> Error.UnboundTypeVariablesError)
unboundTypeVariablesErrorWithType original newVal = Error.UnboundTypeVariablesError {
  Error.unboundTypeVariablesErrorVariables = (Error.unboundTypeVariablesErrorVariables original),
  Error.unboundTypeVariablesErrorType = newVal}

undefinedFieldError :: (Core.Name -> Core.Name -> Error.UndefinedFieldError)
undefinedFieldError fieldName typeName = Error.UndefinedFieldError {
  Error.undefinedFieldErrorFieldName = fieldName,
  Error.undefinedFieldErrorTypeName = typeName}

undefinedFieldErrorFieldName :: (Error.UndefinedFieldError -> Core.Name)
undefinedFieldErrorFieldName = Error.undefinedFieldErrorFieldName

undefinedFieldErrorTypeName :: (Error.UndefinedFieldError -> Core.Name)
undefinedFieldErrorTypeName = Error.undefinedFieldErrorTypeName

undefinedFieldErrorWithFieldName :: (Error.UndefinedFieldError -> Core.Name -> Error.UndefinedFieldError)
undefinedFieldErrorWithFieldName original newVal = Error.UndefinedFieldError {
  Error.undefinedFieldErrorFieldName = newVal,
  Error.undefinedFieldErrorTypeName = (Error.undefinedFieldErrorTypeName original)}

undefinedFieldErrorWithTypeName :: (Error.UndefinedFieldError -> Core.Name -> Error.UndefinedFieldError)
undefinedFieldErrorWithTypeName original newVal = Error.UndefinedFieldError {
  Error.undefinedFieldErrorFieldName = (Error.undefinedFieldErrorFieldName original),
  Error.undefinedFieldErrorTypeName = newVal}

undefinedTermError :: (Core.Name -> Error.UndefinedTermError)
undefinedTermError name = Error.UndefinedTermError {
  Error.undefinedTermErrorName = name}

undefinedTermErrorName :: (Error.UndefinedTermError -> Core.Name)
undefinedTermErrorName = Error.undefinedTermErrorName

undefinedTermErrorWithName :: (t0 -> Core.Name -> Error.UndefinedTermError)
undefinedTermErrorWithName original newVal = Error.UndefinedTermError {
  Error.undefinedTermErrorName = newVal}

undefinedTypeError :: (Core.Name -> Error.UndefinedTypeError)
undefinedTypeError name = Error.UndefinedTypeError {
  Error.undefinedTypeErrorName = name}

undefinedTypeErrorName :: (Error.UndefinedTypeError -> Core.Name)
undefinedTypeErrorName = Error.undefinedTypeErrorName

undefinedTypeErrorWithName :: (t0 -> Core.Name -> Error.UndefinedTypeError)
undefinedTypeErrorWithName original newVal = Error.UndefinedTypeError {
  Error.undefinedTypeErrorName = newVal}

unequalTypesError :: ([Core.Type] -> String -> Error.UnequalTypesError)
unequalTypesError types description = Error.UnequalTypesError {
  Error.unequalTypesErrorTypes = types,
  Error.unequalTypesErrorDescription = description}

unequalTypesErrorTypes :: (Error.UnequalTypesError -> [Core.Type])
unequalTypesErrorTypes = Error.unequalTypesErrorTypes

unequalTypesErrorDescription :: (Error.UnequalTypesError -> String)
unequalTypesErrorDescription = Error.unequalTypesErrorDescription

unequalTypesErrorWithTypes :: (Error.UnequalTypesError -> [Core.Type] -> Error.UnequalTypesError)
unequalTypesErrorWithTypes original newVal = Error.UnequalTypesError {
  Error.unequalTypesErrorTypes = newVal,
  Error.unequalTypesErrorDescription = (Error.unequalTypesErrorDescription original)}

unequalTypesErrorWithDescription :: (Error.UnequalTypesError -> String -> Error.UnequalTypesError)
unequalTypesErrorWithDescription original newVal = Error.UnequalTypesError {
  Error.unequalTypesErrorTypes = (Error.unequalTypesErrorTypes original),
  Error.unequalTypesErrorDescription = newVal}

unexpectedTermVariantError :: (Variants.TermVariant -> Core.Term -> Error.UnexpectedTermVariantError)
unexpectedTermVariantError expectedVariant actualTerm = Error.UnexpectedTermVariantError {
  Error.unexpectedTermVariantErrorExpectedVariant = expectedVariant,
  Error.unexpectedTermVariantErrorActualTerm = actualTerm}

unexpectedTermVariantErrorExpectedVariant :: (Error.UnexpectedTermVariantError -> Variants.TermVariant)
unexpectedTermVariantErrorExpectedVariant = Error.unexpectedTermVariantErrorExpectedVariant

unexpectedTermVariantErrorActualTerm :: (Error.UnexpectedTermVariantError -> Core.Term)
unexpectedTermVariantErrorActualTerm = Error.unexpectedTermVariantErrorActualTerm

unexpectedTermVariantErrorWithExpectedVariant :: (Error.UnexpectedTermVariantError -> Variants.TermVariant -> Error.UnexpectedTermVariantError)
unexpectedTermVariantErrorWithExpectedVariant original newVal = Error.UnexpectedTermVariantError {
  Error.unexpectedTermVariantErrorExpectedVariant = newVal,
  Error.unexpectedTermVariantErrorActualTerm = (Error.unexpectedTermVariantErrorActualTerm original)}

unexpectedTermVariantErrorWithActualTerm :: (Error.UnexpectedTermVariantError -> Core.Term -> Error.UnexpectedTermVariantError)
unexpectedTermVariantErrorWithActualTerm original newVal = Error.UnexpectedTermVariantError {
  Error.unexpectedTermVariantErrorExpectedVariant = (Error.unexpectedTermVariantErrorExpectedVariant original),
  Error.unexpectedTermVariantErrorActualTerm = newVal}

unexpectedTypeVariantError :: (Variants.TypeVariant -> Core.Type -> Error.UnexpectedTypeVariantError)
unexpectedTypeVariantError expectedVariant actualType = Error.UnexpectedTypeVariantError {
  Error.unexpectedTypeVariantErrorExpectedVariant = expectedVariant,
  Error.unexpectedTypeVariantErrorActualType = actualType}

unexpectedTypeVariantErrorExpectedVariant :: (Error.UnexpectedTypeVariantError -> Variants.TypeVariant)
unexpectedTypeVariantErrorExpectedVariant = Error.unexpectedTypeVariantErrorExpectedVariant

unexpectedTypeVariantErrorActualType :: (Error.UnexpectedTypeVariantError -> Core.Type)
unexpectedTypeVariantErrorActualType = Error.unexpectedTypeVariantErrorActualType

unexpectedTypeVariantErrorWithExpectedVariant :: (Error.UnexpectedTypeVariantError -> Variants.TypeVariant -> Error.UnexpectedTypeVariantError)
unexpectedTypeVariantErrorWithExpectedVariant original newVal = Error.UnexpectedTypeVariantError {
  Error.unexpectedTypeVariantErrorExpectedVariant = newVal,
  Error.unexpectedTypeVariantErrorActualType = (Error.unexpectedTypeVariantErrorActualType original)}

unexpectedTypeVariantErrorWithActualType :: (Error.UnexpectedTypeVariantError -> Core.Type -> Error.UnexpectedTypeVariantError)
unexpectedTypeVariantErrorWithActualType original newVal = Error.UnexpectedTypeVariantError {
  Error.unexpectedTypeVariantErrorExpectedVariant = (Error.unexpectedTypeVariantErrorExpectedVariant original),
  Error.unexpectedTypeVariantErrorActualType = newVal}

unificationError :: (Core.Type -> Core.Type -> String -> Error.UnificationError)
unificationError leftType rightType message = Error.UnificationError {
  Error.unificationErrorLeftType = leftType,
  Error.unificationErrorRightType = rightType,
  Error.unificationErrorMessage = message}

unificationErrorLeftType :: (Error.UnificationError -> Core.Type)
unificationErrorLeftType = Error.unificationErrorLeftType

unificationErrorRightType :: (Error.UnificationError -> Core.Type)
unificationErrorRightType = Error.unificationErrorRightType

unificationErrorMessage :: (Error.UnificationError -> String)
unificationErrorMessage = Error.unificationErrorMessage

unificationErrorWithLeftType :: (Error.UnificationError -> Core.Type -> Error.UnificationError)
unificationErrorWithLeftType original newVal = Error.UnificationError {
  Error.unificationErrorLeftType = newVal,
  Error.unificationErrorRightType = (Error.unificationErrorRightType original),
  Error.unificationErrorMessage = (Error.unificationErrorMessage original)}

unificationErrorWithRightType :: (Error.UnificationError -> Core.Type -> Error.UnificationError)
unificationErrorWithRightType original newVal = Error.UnificationError {
  Error.unificationErrorLeftType = (Error.unificationErrorLeftType original),
  Error.unificationErrorRightType = newVal,
  Error.unificationErrorMessage = (Error.unificationErrorMessage original)}

unificationErrorWithMessage :: (Error.UnificationError -> String -> Error.UnificationError)
unificationErrorWithMessage original newVal = Error.UnificationError {
  Error.unificationErrorLeftType = (Error.unificationErrorLeftType original),
  Error.unificationErrorRightType = (Error.unificationErrorRightType original),
  Error.unificationErrorMessage = newVal}

unsupportedTermVariantError :: (Variants.TermVariant -> Error.UnsupportedTermVariantError)
unsupportedTermVariantError termVariant = Error.UnsupportedTermVariantError {
  Error.unsupportedTermVariantErrorTermVariant = termVariant}

unsupportedTermVariantErrorTermVariant :: (Error.UnsupportedTermVariantError -> Variants.TermVariant)
unsupportedTermVariantErrorTermVariant = Error.unsupportedTermVariantErrorTermVariant

unsupportedTermVariantErrorWithTermVariant :: (t0 -> Variants.TermVariant -> Error.UnsupportedTermVariantError)
unsupportedTermVariantErrorWithTermVariant original newVal = Error.UnsupportedTermVariantError {
  Error.unsupportedTermVariantErrorTermVariant = newVal}

untypedLambdaError :: Error.UntypedLambdaError
untypedLambdaError = Error.UntypedLambdaError {
}

untypedLetBindingError :: (Core.Binding -> Error.UntypedLetBindingError)
untypedLetBindingError binding = Error.UntypedLetBindingError {
  Error.untypedLetBindingErrorBinding = binding}

untypedLetBindingErrorBinding :: (Error.UntypedLetBindingError -> Core.Binding)
untypedLetBindingErrorBinding = Error.untypedLetBindingErrorBinding

untypedLetBindingErrorWithBinding :: (t0 -> Core.Binding -> Error.UntypedLetBindingError)
untypedLetBindingErrorWithBinding original newVal = Error.UntypedLetBindingError {
  Error.untypedLetBindingErrorBinding = newVal}
