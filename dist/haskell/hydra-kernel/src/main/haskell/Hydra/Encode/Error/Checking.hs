-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.error.checking

module Hydra.Encode.Error.Checking where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Paths as Paths
import qualified Hydra.Encode.Typing as Typing
import qualified Hydra.Encode.Variants as Variants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

checkingError :: Checking.CheckingError -> Core.Term
checkingError x =
    case x of
      Checking.CheckingErrorIncorrectUnification v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "incorrectUnification"),
          Core.fieldTerm = (incorrectUnificationError v0)}})
      Checking.CheckingErrorNotAForallType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "notAForallType"),
          Core.fieldTerm = (notAForallTypeError v0)}})
      Checking.CheckingErrorNotAFunctionType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "notAFunctionType"),
          Core.fieldTerm = (notAFunctionTypeError v0)}})
      Checking.CheckingErrorOther v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (otherCheckingError v0)}})
      Checking.CheckingErrorTypeArityMismatch v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeArityMismatch"),
          Core.fieldTerm = (typeArityMismatchError v0)}})
      Checking.CheckingErrorTypeMismatch v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeMismatch"),
          Core.fieldTerm = (typeMismatchError v0)}})
      Checking.CheckingErrorUnboundTypeVariables v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unboundTypeVariables"),
          Core.fieldTerm = (unboundTypeVariablesError v0)}})
      Checking.CheckingErrorUndefinedTermVariable v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTermVariable"),
          Core.fieldTerm = (undefinedTermVariableCheckingError v0)}})
      Checking.CheckingErrorUnequalTypes v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unequalTypes"),
          Core.fieldTerm = (unequalTypesError v0)}})
      Checking.CheckingErrorUnsupportedTermVariant v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unsupportedTermVariant"),
          Core.fieldTerm = (unsupportedTermVariantError v0)}})
      Checking.CheckingErrorUntypedLambda v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "untypedLambda"),
          Core.fieldTerm = (untypedLambdaError v0)}})
      Checking.CheckingErrorUntypedLetBinding v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "untypedLetBinding"),
          Core.fieldTerm = (untypedLetBindingError v0)}})
      Checking.CheckingErrorUntypedTermVariable v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "untypedTermVariable"),
          Core.fieldTerm = (untypedTermVariableCheckingError v0)}})

incorrectUnificationError :: Checking.IncorrectUnificationError -> Core.Term
incorrectUnificationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.IncorrectUnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Typing.typeSubst (Checking.incorrectUnificationErrorSubstitution x))}]})

notAForallTypeError :: Checking.NotAForallTypeError -> Core.Term
notAForallTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (EncodeCore.type_ (Checking.notAForallTypeErrorType x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map EncodeCore.type_ xs)) (Checking.notAForallTypeErrorTypeArguments x))}]})

notAFunctionTypeError :: Checking.NotAFunctionTypeError -> Core.Term
notAFunctionTypeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAFunctionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (EncodeCore.type_ (Checking.notAFunctionTypeErrorType x))}]})

otherCheckingError :: Checking.OtherCheckingError -> Core.Term
otherCheckingError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Paths.subtermPath (Checking.otherCheckingErrorPath x))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Checking.otherCheckingErrorMessage x))}]})

typeArityMismatchError :: Checking.TypeArityMismatchError -> Core.Term
typeArityMismatchError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (EncodeCore.type_ (Checking.typeArityMismatchErrorType x))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Checking.typeArityMismatchErrorExpectedArity x))},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Checking.typeArityMismatchErrorActualArity x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map EncodeCore.type_ xs)) (Checking.typeArityMismatchErrorTypeArguments x))}]})

typeMismatchError :: Checking.TypeMismatchError -> Core.Term
typeMismatchError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (EncodeCore.type_ (Checking.typeMismatchErrorExpectedType x))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (EncodeCore.type_ (Checking.typeMismatchErrorActualType x))}]})

unboundTypeVariablesError :: Checking.UnboundTypeVariablesError -> Core.Term
unboundTypeVariablesError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = ((\s -> Core.TermSet (Sets.map EncodeCore.name s)) (Checking.unboundTypeVariablesErrorVariables x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (EncodeCore.type_ (Checking.unboundTypeVariablesErrorType x))}]})

undefinedTermVariableCheckingError :: Checking.UndefinedTermVariableCheckingError -> Core.Term
undefinedTermVariableCheckingError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Paths.subtermPath (Checking.undefinedTermVariableCheckingErrorPath x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (Checking.undefinedTermVariableCheckingErrorName x))}]})

unequalTypesError :: Checking.UnequalTypesError -> Core.Term
unequalTypesError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map EncodeCore.type_ xs)) (Checking.unequalTypesErrorTypes x))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Checking.unequalTypesErrorDescription x))}]})

unsupportedTermVariantError :: Checking.UnsupportedTermVariantError -> Core.Term
unsupportedTermVariantError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnsupportedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "termVariant"),
          Core.fieldTerm = (Variants.termVariant (Checking.unsupportedTermVariantErrorTermVariant x))}]})

untypedLambdaError :: t0 -> Core.Term
untypedLambdaError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLambdaError"),
      Core.recordFields = []})

untypedLetBindingError :: Checking.UntypedLetBindingError -> Core.Term
untypedLetBindingError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLetBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binding"),
          Core.fieldTerm = (EncodeCore.binding (Checking.untypedLetBindingErrorBinding x))}]})

untypedTermVariableCheckingError :: Checking.UntypedTermVariableCheckingError -> Core.Term
untypedTermVariableCheckingError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Paths.subtermPath (Checking.untypedTermVariableCheckingErrorPath x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (Checking.untypedTermVariableCheckingErrorName x))}]})
