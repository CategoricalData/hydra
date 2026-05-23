-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.error.checking

module Hydra.Dsl.Error.Checking where
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Typing as Typing
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | DSL injection for the incorrectUnification variant of hydra.error.checking.CheckingError
checkingErrorIncorrectUnification :: Phantoms.TTerm Checking.IncorrectUnificationError -> Phantoms.TTerm Checking.CheckingError
checkingErrorIncorrectUnification x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "incorrectUnification"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the notAForallType variant of hydra.error.checking.CheckingError
checkingErrorNotAForallType :: Phantoms.TTerm Checking.NotAForallTypeError -> Phantoms.TTerm Checking.CheckingError
checkingErrorNotAForallType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notAForallType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the notAFunctionType variant of hydra.error.checking.CheckingError
checkingErrorNotAFunctionType :: Phantoms.TTerm Checking.NotAFunctionTypeError -> Phantoms.TTerm Checking.CheckingError
checkingErrorNotAFunctionType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notAFunctionType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the other variant of hydra.error.checking.CheckingError
checkingErrorOther :: Phantoms.TTerm Checking.OtherCheckingError -> Phantoms.TTerm Checking.CheckingError
checkingErrorOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeArityMismatch variant of hydra.error.checking.CheckingError
checkingErrorTypeArityMismatch :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Checking.CheckingError
checkingErrorTypeArityMismatch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeArityMismatch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeMismatch variant of hydra.error.checking.CheckingError
checkingErrorTypeMismatch :: Phantoms.TTerm Checking.TypeMismatchError -> Phantoms.TTerm Checking.CheckingError
checkingErrorTypeMismatch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeMismatch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unboundTypeVariables variant of hydra.error.checking.CheckingError
checkingErrorUnboundTypeVariables :: Phantoms.TTerm Checking.UnboundTypeVariablesError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUnboundTypeVariables x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unboundTypeVariables"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the undefinedTermVariable variant of hydra.error.checking.CheckingError
checkingErrorUndefinedTermVariable :: Phantoms.TTerm Checking.UndefinedTermVariableCheckingError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUndefinedTermVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unequalTypes variant of hydra.error.checking.CheckingError
checkingErrorUnequalTypes :: Phantoms.TTerm Checking.UnequalTypesError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUnequalTypes x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unequalTypes"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unsupportedTermVariant variant of hydra.error.checking.CheckingError
checkingErrorUnsupportedTermVariant :: Phantoms.TTerm Checking.UnsupportedTermVariantError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUnsupportedTermVariant x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsupportedTermVariant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the untypedLambda variant of hydra.error.checking.CheckingError
checkingErrorUntypedLambda :: Phantoms.TTerm Checking.UntypedLambdaError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUntypedLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedLambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the untypedLetBinding variant of hydra.error.checking.CheckingError
checkingErrorUntypedLetBinding :: Phantoms.TTerm Checking.UntypedLetBindingError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUntypedLetBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedLetBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the untypedTermVariable variant of hydra.error.checking.CheckingError
checkingErrorUntypedTermVariable :: Phantoms.TTerm Checking.UntypedTermVariableCheckingError -> Phantoms.TTerm Checking.CheckingError
checkingErrorUntypedTermVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.error.checking.IncorrectUnificationError
incorrectUnificationError :: Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm Checking.IncorrectUnificationError
incorrectUnificationError substitution =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.IncorrectUnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Phantoms.unTTerm substitution)}]}))
-- | DSL accessor for the substitution field of hydra.error.checking.IncorrectUnificationError
incorrectUnificationErrorSubstitution :: Phantoms.TTerm Checking.IncorrectUnificationError -> Phantoms.TTerm Typing.TypeSubst
incorrectUnificationErrorSubstitution x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.IncorrectUnificationError"),
        Core.projectionFieldName = (Core.Name "substitution")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the substitution field of hydra.error.checking.IncorrectUnificationError
incorrectUnificationErrorWithSubstitution :: Phantoms.TTerm Checking.IncorrectUnificationError -> Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm Checking.IncorrectUnificationError
incorrectUnificationErrorWithSubstitution original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.IncorrectUnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.NotAForallTypeError
notAForallTypeError :: Phantoms.TTerm Core.Type -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Checking.NotAForallTypeError
notAForallTypeError type_ typeArguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)}]}))
-- | DSL accessor for the type field of hydra.error.checking.NotAForallTypeError
notAForallTypeErrorType :: Phantoms.TTerm Checking.NotAForallTypeError -> Phantoms.TTerm Core.Type
notAForallTypeErrorType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.error.checking.NotAForallTypeError
notAForallTypeErrorTypeArguments :: Phantoms.TTerm Checking.NotAForallTypeError -> Phantoms.TTerm [Core.Type]
notAForallTypeErrorTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the type field of hydra.error.checking.NotAForallTypeError
notAForallTypeErrorWithType :: Phantoms.TTerm Checking.NotAForallTypeError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.NotAForallTypeError
notAForallTypeErrorWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.error.checking.NotAForallTypeError
notAForallTypeErrorWithTypeArguments :: Phantoms.TTerm Checking.NotAForallTypeError -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Checking.NotAForallTypeError
notAForallTypeErrorWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.NotAFunctionTypeError
notAFunctionTypeError :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.NotAFunctionTypeError
notAFunctionTypeError type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAFunctionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the type field of hydra.error.checking.NotAFunctionTypeError
notAFunctionTypeErrorType :: Phantoms.TTerm Checking.NotAFunctionTypeError -> Phantoms.TTerm Core.Type
notAFunctionTypeErrorType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAFunctionTypeError"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the type field of hydra.error.checking.NotAFunctionTypeError
notAFunctionTypeErrorWithType :: Phantoms.TTerm Checking.NotAFunctionTypeError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.NotAFunctionTypeError
notAFunctionTypeErrorWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAFunctionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.OtherCheckingError
otherCheckingError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm String -> Phantoms.TTerm Checking.OtherCheckingError
otherCheckingError path message =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm message)}]}))
-- | DSL accessor for the message field of hydra.error.checking.OtherCheckingError
otherCheckingErrorMessage :: Phantoms.TTerm Checking.OtherCheckingError -> Phantoms.TTerm String
otherCheckingErrorMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
        Core.projectionFieldName = (Core.Name "message")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the path field of hydra.error.checking.OtherCheckingError
otherCheckingErrorPath :: Phantoms.TTerm Checking.OtherCheckingError -> Phantoms.TTerm Paths.SubtermPath
otherCheckingErrorPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the message field of hydra.error.checking.OtherCheckingError
otherCheckingErrorWithMessage :: Phantoms.TTerm Checking.OtherCheckingError -> Phantoms.TTerm String -> Phantoms.TTerm Checking.OtherCheckingError
otherCheckingErrorWithMessage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the path field of hydra.error.checking.OtherCheckingError
otherCheckingErrorWithPath :: Phantoms.TTerm Checking.OtherCheckingError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Checking.OtherCheckingError
otherCheckingErrorWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.error.checking.TypeArityMismatchError
typeArityMismatchError :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Checking.TypeArityMismatchError
typeArityMismatchError type_ expectedArity actualArity typeArguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Phantoms.unTTerm expectedArity)},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Phantoms.unTTerm actualArity)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm typeArguments)}]}))
-- | DSL accessor for the actualArity field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorActualArity :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Int
typeArityMismatchErrorActualArity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionFieldName = (Core.Name "actualArity")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expectedArity field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorExpectedArity :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Int
typeArityMismatchErrorExpectedArity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionFieldName = (Core.Name "expectedArity")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorType :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Core.Type
typeArityMismatchErrorType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorTypeArguments :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm [Core.Type]
typeArityMismatchErrorTypeArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the actualArity field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorWithActualArity :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Int -> Phantoms.TTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithActualArity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "expectedArity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the expectedArity field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorWithExpectedArity :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Int -> Phantoms.TTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithExpectedArity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "actualArity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorWithType :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "expectedArity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "actualArity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorWithTypeArguments :: Phantoms.TTerm Checking.TypeArityMismatchError -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithTypeArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "expectedArity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "actualArity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.TypeMismatchError
typeMismatchError :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.TypeMismatchError
typeMismatchError expectedType actualType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Phantoms.unTTerm expectedType)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Phantoms.unTTerm actualType)}]}))
-- | DSL accessor for the actualType field of hydra.error.checking.TypeMismatchError
typeMismatchErrorActualType :: Phantoms.TTerm Checking.TypeMismatchError -> Phantoms.TTerm Core.Type
typeMismatchErrorActualType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
        Core.projectionFieldName = (Core.Name "actualType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expectedType field of hydra.error.checking.TypeMismatchError
typeMismatchErrorExpectedType :: Phantoms.TTerm Checking.TypeMismatchError -> Phantoms.TTerm Core.Type
typeMismatchErrorExpectedType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
        Core.projectionFieldName = (Core.Name "expectedType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the actualType field of hydra.error.checking.TypeMismatchError
typeMismatchErrorWithActualType :: Phantoms.TTerm Checking.TypeMismatchError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.TypeMismatchError
typeMismatchErrorWithActualType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
              Core.projectionFieldName = (Core.Name "expectedType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expectedType field of hydra.error.checking.TypeMismatchError
typeMismatchErrorWithExpectedType :: Phantoms.TTerm Checking.TypeMismatchError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.TypeMismatchError
typeMismatchErrorWithExpectedType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
              Core.projectionFieldName = (Core.Name "actualType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.error.checking.UnboundTypeVariablesError
unboundTypeVariablesError :: Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.UnboundTypeVariablesError
unboundTypeVariablesError variables type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm variables)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the type field of hydra.error.checking.UnboundTypeVariablesError
unboundTypeVariablesErrorType :: Phantoms.TTerm Checking.UnboundTypeVariablesError -> Phantoms.TTerm Core.Type
unboundTypeVariablesErrorType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variables field of hydra.error.checking.UnboundTypeVariablesError
unboundTypeVariablesErrorVariables :: Phantoms.TTerm Checking.UnboundTypeVariablesError -> Phantoms.TTerm (S.Set Core.Name)
unboundTypeVariablesErrorVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
        Core.projectionFieldName = (Core.Name "variables")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the type field of hydra.error.checking.UnboundTypeVariablesError
unboundTypeVariablesErrorWithType :: Phantoms.TTerm Checking.UnboundTypeVariablesError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Checking.UnboundTypeVariablesError
unboundTypeVariablesErrorWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the variables field of hydra.error.checking.UnboundTypeVariablesError
unboundTypeVariablesErrorWithVariables :: Phantoms.TTerm Checking.UnboundTypeVariablesError -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Checking.UnboundTypeVariablesError
unboundTypeVariablesErrorWithVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.error.checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingError path name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.error.checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorName :: Phantoms.TTerm Checking.UndefinedTermVariableCheckingError -> Phantoms.TTerm Core.Name
undefinedTermVariableCheckingErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the path field of hydra.error.checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorPath :: Phantoms.TTerm Checking.UndefinedTermVariableCheckingError -> Phantoms.TTerm Paths.SubtermPath
undefinedTermVariableCheckingErrorPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.error.checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorWithName :: Phantoms.TTerm Checking.UndefinedTermVariableCheckingError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the path field of hydra.error.checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorWithPath :: Phantoms.TTerm Checking.UndefinedTermVariableCheckingError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.error.checking.UnequalTypesError
unequalTypesError :: Phantoms.TTerm [Core.Type] -> Phantoms.TTerm String -> Phantoms.TTerm Checking.UnequalTypesError
unequalTypesError types description =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)}]}))
-- | DSL accessor for the description field of hydra.error.checking.UnequalTypesError
unequalTypesErrorDescription :: Phantoms.TTerm Checking.UnequalTypesError -> Phantoms.TTerm String
unequalTypesErrorDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the types field of hydra.error.checking.UnequalTypesError
unequalTypesErrorTypes :: Phantoms.TTerm Checking.UnequalTypesError -> Phantoms.TTerm [Core.Type]
unequalTypesErrorTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
        Core.projectionFieldName = (Core.Name "types")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the description field of hydra.error.checking.UnequalTypesError
unequalTypesErrorWithDescription :: Phantoms.TTerm Checking.UnequalTypesError -> Phantoms.TTerm String -> Phantoms.TTerm Checking.UnequalTypesError
unequalTypesErrorWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the types field of hydra.error.checking.UnequalTypesError
unequalTypesErrorWithTypes :: Phantoms.TTerm Checking.UnequalTypesError -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm Checking.UnequalTypesError
unequalTypesErrorWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.error.checking.UnsupportedTermVariantError
unsupportedTermVariantError :: Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Checking.UnsupportedTermVariantError
unsupportedTermVariantError termVariant =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnsupportedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "termVariant"),
          Core.fieldTerm = (Phantoms.unTTerm termVariant)}]}))
-- | DSL accessor for the termVariant field of hydra.error.checking.UnsupportedTermVariantError
unsupportedTermVariantErrorTermVariant :: Phantoms.TTerm Checking.UnsupportedTermVariantError -> Phantoms.TTerm Variants.TermVariant
unsupportedTermVariantErrorTermVariant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnsupportedTermVariantError"),
        Core.projectionFieldName = (Core.Name "termVariant")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the termVariant field of hydra.error.checking.UnsupportedTermVariantError
unsupportedTermVariantErrorWithTermVariant :: Phantoms.TTerm Checking.UnsupportedTermVariantError -> Phantoms.TTerm Variants.TermVariant -> Phantoms.TTerm Checking.UnsupportedTermVariantError
unsupportedTermVariantErrorWithTermVariant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnsupportedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "termVariant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.UntypedLambdaError
untypedLambdaError :: Phantoms.TTerm Checking.UntypedLambdaError
untypedLambdaError =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLambdaError"),
      Core.recordFields = []}))
-- | DSL constructor for hydra.error.checking.UntypedLetBindingError
untypedLetBindingError :: Phantoms.TTerm Core.Binding -> Phantoms.TTerm Checking.UntypedLetBindingError
untypedLetBindingError binding =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLetBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binding"),
          Core.fieldTerm = (Phantoms.unTTerm binding)}]}))
-- | DSL accessor for the binding field of hydra.error.checking.UntypedLetBindingError
untypedLetBindingErrorBinding :: Phantoms.TTerm Checking.UntypedLetBindingError -> Phantoms.TTerm Core.Binding
untypedLetBindingErrorBinding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedLetBindingError"),
        Core.projectionFieldName = (Core.Name "binding")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the binding field of hydra.error.checking.UntypedLetBindingError
untypedLetBindingErrorWithBinding :: Phantoms.TTerm Checking.UntypedLetBindingError -> Phantoms.TTerm Core.Binding -> Phantoms.TTerm Checking.UntypedLetBindingError
untypedLetBindingErrorWithBinding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLetBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingError path name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.error.checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorName :: Phantoms.TTerm Checking.UntypedTermVariableCheckingError -> Phantoms.TTerm Core.Name
untypedTermVariableCheckingErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the path field of hydra.error.checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorPath :: Phantoms.TTerm Checking.UntypedTermVariableCheckingError -> Phantoms.TTerm Paths.SubtermPath
untypedTermVariableCheckingErrorPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.error.checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorWithName :: Phantoms.TTerm Checking.UntypedTermVariableCheckingError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the path field of hydra.error.checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorWithPath :: Phantoms.TTerm Checking.UntypedTermVariableCheckingError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
