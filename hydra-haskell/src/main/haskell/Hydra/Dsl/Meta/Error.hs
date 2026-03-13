-- | Meta-DSL for constructing error-related terms (DecodingError, OtherError, UnificationError, Error, etc.)

module Hydra.Dsl.Meta.Error where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms

import qualified Data.Set as S


-- CheckingError constructors

checkingIncorrectUnification :: TTerm IncorrectUnificationError -> TTerm CheckingError
checkingIncorrectUnification = inject _CheckingError _CheckingError_incorrectUnification

checkingNotAForallType :: TTerm NotAForallTypeError -> TTerm CheckingError
checkingNotAForallType = inject _CheckingError _CheckingError_notAForallType

checkingNotAFunctionType :: TTerm NotAFunctionTypeError -> TTerm CheckingError
checkingNotAFunctionType = inject _CheckingError _CheckingError_notAFunctionType

checkingTypeArityMismatch :: TTerm TypeArityMismatchError -> TTerm CheckingError
checkingTypeArityMismatch = inject _CheckingError _CheckingError_typeArityMismatch

checkingTypeMismatch :: TTerm TypeMismatchError -> TTerm CheckingError
checkingTypeMismatch = inject _CheckingError _CheckingError_typeMismatch

checkingUnboundTypeVariables :: TTerm UnboundTypeVariablesError -> TTerm CheckingError
checkingUnboundTypeVariables = inject _CheckingError _CheckingError_unboundTypeVariables

checkingUnequalTypes :: TTerm UnequalTypesError -> TTerm CheckingError
checkingUnequalTypes = inject _CheckingError _CheckingError_unequalTypes

checkingUnsupportedTermVariant :: TTerm UnsupportedTermVariantError -> TTerm CheckingError
checkingUnsupportedTermVariant = inject _CheckingError _CheckingError_unsupportedTermVariant

checkingUntypedLambda :: TTerm UntypedLambdaError -> TTerm CheckingError
checkingUntypedLambda = inject _CheckingError _CheckingError_untypedLambda

checkingUntypedLetBinding :: TTerm UntypedLetBindingError -> TTerm CheckingError
checkingUntypedLetBinding = inject _CheckingError _CheckingError_untypedLetBinding

-- DecodingError

decodingError :: TTerm String -> TTerm DecodingError
decodingError = wrap _DecodingError

unDecodingError :: TTerm (DecodingError -> String)
unDecodingError = unwrap _DecodingError

-- DuplicateBindingError

duplicateBindingError :: TTerm Name -> TTerm DuplicateBindingError
duplicateBindingError n = record _DuplicateBindingError [
    _DuplicateBindingError_name>>: n]

-- DuplicateFieldError

duplicateFieldError :: TTerm Name -> TTerm DuplicateFieldError
duplicateFieldError n = record _DuplicateFieldError [
    _DuplicateFieldError_name>>: n]

-- IncorrectUnificationError

incorrectUnificationError :: TTerm TypeSubst -> TTerm IncorrectUnificationError
incorrectUnificationError s = record _IncorrectUnificationError [
    _IncorrectUnificationError_substitution>>: s]

-- NotAForallTypeError

notAForallTypeError :: TTerm Type -> TTerm [Type] -> TTerm NotAForallTypeError
notAForallTypeError t args = record _NotAForallTypeError [
    _NotAForallTypeError_type>>: t,
    _NotAForallTypeError_typeArguments>>: args]

-- NotAFunctionTypeError

notAFunctionTypeError :: TTerm Type -> TTerm NotAFunctionTypeError
notAFunctionTypeError t = record _NotAFunctionTypeError [
    _NotAFunctionTypeError_type>>: t]

-- OtherError

otherError :: TTerm String -> TTerm OtherError
otherError = wrap _OtherError

unOtherError :: TTerm (OtherError -> String)
unOtherError = unwrap _OtherError

-- TypeArityMismatchError

typeArityMismatchError :: TTerm Type -> TTerm Int -> TTerm Int -> TTerm [Type] -> TTerm TypeArityMismatchError
typeArityMismatchError t expected actual args = record _TypeArityMismatchError [
    _TypeArityMismatchError_type>>: t,
    _TypeArityMismatchError_expectedArity>>: expected,
    _TypeArityMismatchError_actualArity>>: actual,
    _TypeArityMismatchError_typeArguments>>: args]

-- TypeMismatchError

typeMismatchError :: TTerm Type -> TTerm Type -> TTerm TypeMismatchError
typeMismatchError expected actual = record _TypeMismatchError [
    _TypeMismatchError_expectedType>>: expected,
    _TypeMismatchError_actualType>>: actual]

-- UnboundTypeVariablesError

unboundTypeVariablesError :: TTerm (S.Set Name) -> TTerm Type -> TTerm UnboundTypeVariablesError
unboundTypeVariablesError vars t = record _UnboundTypeVariablesError [
    _UnboundTypeVariablesError_variables>>: vars,
    _UnboundTypeVariablesError_type>>: t]

-- UndefinedFieldError

undefinedFieldError :: TTerm Name -> TTerm Name -> TTerm UndefinedFieldError
undefinedFieldError fname tname = record _UndefinedFieldError [
    _UndefinedFieldError_fieldName>>: fname,
    _UndefinedFieldError_typeName>>: tname]

-- UndefinedTermError

undefinedTermError :: TTerm Name -> TTerm UndefinedTermError
undefinedTermError n = record _UndefinedTermError [
    _UndefinedTermError_name>>: n]

-- UndefinedTypeError

undefinedTypeError :: TTerm Name -> TTerm UndefinedTypeError
undefinedTypeError n = record _UndefinedTypeError [
    _UndefinedTypeError_name>>: n]

-- UnequalTypesError

unequalTypesError :: TTerm [Type] -> TTerm String -> TTerm UnequalTypesError
unequalTypesError types desc = record _UnequalTypesError [
    _UnequalTypesError_types>>: types,
    _UnequalTypesError_description>>: desc]

-- UnexpectedTermVariantError

unexpectedTermVariantError :: TTerm TermVariant -> TTerm Term -> TTerm UnexpectedTermVariantError
unexpectedTermVariantError v t = record _UnexpectedTermVariantError [
    _UnexpectedTermVariantError_expectedVariant>>: v,
    _UnexpectedTermVariantError_actualTerm>>: t]

-- UnexpectedTypeVariantError

unexpectedTypeVariantError :: TTerm TypeVariant -> TTerm Type -> TTerm UnexpectedTypeVariantError
unexpectedTypeVariantError v t = record _UnexpectedTypeVariantError [
    _UnexpectedTypeVariantError_expectedVariant>>: v,
    _UnexpectedTypeVariantError_actualType>>: t]

-- UnificationError

unificationError :: TTerm Type -> TTerm Type -> TTerm String -> TTerm UnificationError
unificationError lt rt msg = record _UnificationError [
    _UnificationError_leftType>>: lt,
    _UnificationError_rightType>>: rt,
    _UnificationError_message>>: msg]

unificationErrorLeftType :: TTerm UnificationError -> TTerm Type
unificationErrorLeftType ue = project _UnificationError _UnificationError_leftType @@ ue

unificationErrorRightType :: TTerm UnificationError -> TTerm Type
unificationErrorRightType ue = project _UnificationError _UnificationError_rightType @@ ue

unificationErrorMessage :: TTerm UnificationError -> TTerm String
unificationErrorMessage ue = project _UnificationError _UnificationError_message @@ ue

-- UnsupportedTermVariantError

unsupportedTermVariantError :: TTerm TermVariant -> TTerm UnsupportedTermVariantError
unsupportedTermVariantError v = record _UnsupportedTermVariantError [
    _UnsupportedTermVariantError_termVariant>>: v]

-- UntypedLambdaError

untypedLambdaError :: TTerm UntypedLambdaError
untypedLambdaError = record _UntypedLambdaError []

-- UntypedLetBindingError

untypedLetBindingError :: TTerm Binding -> TTerm UntypedLetBindingError
untypedLetBindingError b = record _UntypedLetBindingError [
    _UntypedLetBindingError_binding>>: b]

-- Error union constructors

errorChecking :: TTerm CheckingError -> TTerm Error
errorChecking = inject _Error _Error_checking

errorDecoding :: TTerm DecodingError -> TTerm Error
errorDecoding = inject _Error _Error_decoding

errorDuplicateBinding :: TTerm DuplicateBindingError -> TTerm Error
errorDuplicateBinding = inject _Error _Error_duplicateBinding

errorDuplicateField :: TTerm DuplicateFieldError -> TTerm Error
errorDuplicateField = inject _Error _Error_duplicateField

errorOther :: TTerm OtherError -> TTerm Error
errorOther = inject _Error _Error_other

errorUndefinedField :: TTerm UndefinedFieldError -> TTerm Error
errorUndefinedField = inject _Error _Error_undefinedField

errorUndefinedTerm :: TTerm UndefinedTermError -> TTerm Error
errorUndefinedTerm = inject _Error _Error_undefinedTerm

errorUndefinedType :: TTerm UndefinedTypeError -> TTerm Error
errorUndefinedType = inject _Error _Error_undefinedType

errorUnexpectedTermVariant :: TTerm UnexpectedTermVariantError -> TTerm Error
errorUnexpectedTermVariant = inject _Error _Error_unexpectedTermVariant

errorUnexpectedTypeVariant :: TTerm UnexpectedTypeVariantError -> TTerm Error
errorUnexpectedTypeVariant = inject _Error _Error_unexpectedTypeVariant

errorUnification :: TTerm UnificationError -> TTerm Error
errorUnification = inject _Error _Error_unification
