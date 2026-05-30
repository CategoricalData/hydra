-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.error.checking

module Hydra.Dsl.Error.Checking where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Paths as Paths
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | DSL injection for the incorrectUnification variant of hydra.error.checking.CheckingError
checkingErrorIncorrectUnification :: Typed.TypedTerm Checking.IncorrectUnificationError -> Typed.TypedTerm Checking.CheckingError
checkingErrorIncorrectUnification x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "incorrectUnification"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the notAForallType variant of hydra.error.checking.CheckingError
checkingErrorNotAForallType :: Typed.TypedTerm Checking.NotAForallTypeError -> Typed.TypedTerm Checking.CheckingError
checkingErrorNotAForallType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notAForallType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the notAFunctionType variant of hydra.error.checking.CheckingError
checkingErrorNotAFunctionType :: Typed.TypedTerm Checking.NotAFunctionTypeError -> Typed.TypedTerm Checking.CheckingError
checkingErrorNotAFunctionType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notAFunctionType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the other variant of hydra.error.checking.CheckingError
checkingErrorOther :: Typed.TypedTerm Checking.OtherCheckingError -> Typed.TypedTerm Checking.CheckingError
checkingErrorOther x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeArityMismatch variant of hydra.error.checking.CheckingError
checkingErrorTypeArityMismatch :: Typed.TypedTerm Checking.TypeArityMismatchError -> Typed.TypedTerm Checking.CheckingError
checkingErrorTypeArityMismatch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeArityMismatch"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeMismatch variant of hydra.error.checking.CheckingError
checkingErrorTypeMismatch :: Typed.TypedTerm Checking.TypeMismatchError -> Typed.TypedTerm Checking.CheckingError
checkingErrorTypeMismatch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeMismatch"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unboundTypeVariables variant of hydra.error.checking.CheckingError
checkingErrorUnboundTypeVariables :: Typed.TypedTerm Checking.UnboundTypeVariablesError -> Typed.TypedTerm Checking.CheckingError
checkingErrorUnboundTypeVariables x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unboundTypeVariables"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the undefinedTermVariable variant of hydra.error.checking.CheckingError
checkingErrorUndefinedTermVariable :: Typed.TypedTerm Checking.UndefinedTermVariableCheckingError -> Typed.TypedTerm Checking.CheckingError
checkingErrorUndefinedTermVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTermVariable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unequalTypes variant of hydra.error.checking.CheckingError
checkingErrorUnequalTypes :: Typed.TypedTerm Checking.UnequalTypesError -> Typed.TypedTerm Checking.CheckingError
checkingErrorUnequalTypes x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unequalTypes"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unsupportedTermVariant variant of hydra.error.checking.CheckingError
checkingErrorUnsupportedTermVariant :: Typed.TypedTerm Checking.UnsupportedTermVariantError -> Typed.TypedTerm Checking.CheckingError
checkingErrorUnsupportedTermVariant x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsupportedTermVariant"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the untypedLambda variant of hydra.error.checking.CheckingError
checkingErrorUntypedLambda :: Typed.TypedTerm Checking.UntypedLambdaError -> Typed.TypedTerm Checking.CheckingError
checkingErrorUntypedLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedLambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the untypedLetBinding variant of hydra.error.checking.CheckingError
checkingErrorUntypedLetBinding :: Typed.TypedTerm Checking.UntypedLetBindingError -> Typed.TypedTerm Checking.CheckingError
checkingErrorUntypedLetBinding x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedLetBinding"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the untypedTermVariable variant of hydra.error.checking.CheckingError
checkingErrorUntypedTermVariable :: Typed.TypedTerm Checking.UntypedTermVariableCheckingError -> Typed.TypedTerm Checking.CheckingError
checkingErrorUntypedTermVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.error.checking.CheckingError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedTermVariable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.error.checking.IncorrectUnificationError
incorrectUnificationError :: Typed.TypedTerm Typing.TypeSubst -> Typed.TypedTerm Checking.IncorrectUnificationError
incorrectUnificationError substitution =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.IncorrectUnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Typed.unTypedTerm substitution)}]}))
-- | DSL accessor for the substitution field of hydra.error.checking.IncorrectUnificationError
incorrectUnificationErrorSubstitution :: Typed.TypedTerm Checking.IncorrectUnificationError -> Typed.TypedTerm Typing.TypeSubst
incorrectUnificationErrorSubstitution x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.IncorrectUnificationError"),
        Core.projectionFieldName = (Core.Name "substitution")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the substitution field of hydra.error.checking.IncorrectUnificationError
incorrectUnificationErrorWithSubstitution :: Typed.TypedTerm Checking.IncorrectUnificationError -> Typed.TypedTerm Typing.TypeSubst -> Typed.TypedTerm Checking.IncorrectUnificationError
incorrectUnificationErrorWithSubstitution original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.IncorrectUnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.NotAForallTypeError
notAForallTypeError :: Typed.TypedTerm Core.Type -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Checking.NotAForallTypeError
notAForallTypeError type_ typeArguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)}]}))
-- | DSL accessor for the type field of hydra.error.checking.NotAForallTypeError
notAForallTypeErrorType :: Typed.TypedTerm Checking.NotAForallTypeError -> Typed.TypedTerm Core.Type
notAForallTypeErrorType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.error.checking.NotAForallTypeError
notAForallTypeErrorTypeArguments :: Typed.TypedTerm Checking.NotAForallTypeError -> Typed.TypedTerm [Core.Type]
notAForallTypeErrorTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the type field of hydra.error.checking.NotAForallTypeError
notAForallTypeErrorWithType :: Typed.TypedTerm Checking.NotAForallTypeError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Checking.NotAForallTypeError
notAForallTypeErrorWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.error.checking.NotAForallTypeError
notAForallTypeErrorWithTypeArguments :: Typed.TypedTerm Checking.NotAForallTypeError -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Checking.NotAForallTypeError
notAForallTypeErrorWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAForallTypeError"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.NotAFunctionTypeError
notAFunctionTypeError :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Checking.NotAFunctionTypeError
notAFunctionTypeError type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAFunctionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the type field of hydra.error.checking.NotAFunctionTypeError
notAFunctionTypeErrorType :: Typed.TypedTerm Checking.NotAFunctionTypeError -> Typed.TypedTerm Core.Type
notAFunctionTypeErrorType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.NotAFunctionTypeError"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the type field of hydra.error.checking.NotAFunctionTypeError
notAFunctionTypeErrorWithType :: Typed.TypedTerm Checking.NotAFunctionTypeError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Checking.NotAFunctionTypeError
notAFunctionTypeErrorWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.NotAFunctionTypeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.OtherCheckingError
otherCheckingError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm String -> Typed.TypedTerm Checking.OtherCheckingError
otherCheckingError path message =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Typed.unTypedTerm message)}]}))
-- | DSL accessor for the message field of hydra.error.checking.OtherCheckingError
otherCheckingErrorMessage :: Typed.TypedTerm Checking.OtherCheckingError -> Typed.TypedTerm String
otherCheckingErrorMessage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
        Core.projectionFieldName = (Core.Name "message")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the path field of hydra.error.checking.OtherCheckingError
otherCheckingErrorPath :: Typed.TypedTerm Checking.OtherCheckingError -> Typed.TypedTerm Paths.SubtermPath
otherCheckingErrorPath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the message field of hydra.error.checking.OtherCheckingError
otherCheckingErrorWithMessage :: Typed.TypedTerm Checking.OtherCheckingError -> Typed.TypedTerm String -> Typed.TypedTerm Checking.OtherCheckingError
otherCheckingErrorWithMessage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the path field of hydra.error.checking.OtherCheckingError
otherCheckingErrorWithPath :: Typed.TypedTerm Checking.OtherCheckingError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Checking.OtherCheckingError
otherCheckingErrorWithPath original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.OtherCheckingError"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.checking.TypeArityMismatchError
typeArityMismatchError :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Int -> Typed.TypedTerm Int -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Checking.TypeArityMismatchError
typeArityMismatchError type_ expectedArity actualArity typeArguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Typed.unTypedTerm expectedArity)},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Typed.unTypedTerm actualArity)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm typeArguments)}]}))
-- | DSL accessor for the actualArity field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorActualArity :: Typed.TypedTerm Checking.TypeArityMismatchError -> Typed.TypedTerm Int
typeArityMismatchErrorActualArity x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionFieldName = (Core.Name "actualArity")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expectedArity field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorExpectedArity :: Typed.TypedTerm Checking.TypeArityMismatchError -> Typed.TypedTerm Int
typeArityMismatchErrorExpectedArity x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionFieldName = (Core.Name "expectedArity")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorType :: Typed.TypedTerm Checking.TypeArityMismatchError -> Typed.TypedTerm Core.Type
typeArityMismatchErrorType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArguments field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorTypeArguments :: Typed.TypedTerm Checking.TypeArityMismatchError -> Typed.TypedTerm [Core.Type]
typeArityMismatchErrorTypeArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
        Core.projectionFieldName = (Core.Name "typeArguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the actualArity field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorWithActualArity :: Typed.TypedTerm Checking.TypeArityMismatchError -> Typed.TypedTerm Int -> Typed.TypedTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithActualArity original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "expectedArity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the expectedArity field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorWithExpectedArity :: Typed.TypedTerm Checking.TypeArityMismatchError -> Typed.TypedTerm Int -> Typed.TypedTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithExpectedArity original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "actualArity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorWithType :: Typed.TypedTerm Checking.TypeArityMismatchError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "expectedArity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "actualArity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "typeArguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArguments field of hydra.error.checking.TypeArityMismatchError
typeArityMismatchErrorWithTypeArguments :: Typed.TypedTerm Checking.TypeArityMismatchError -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Checking.TypeArityMismatchError
typeArityMismatchErrorWithTypeArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expectedArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "expectedArity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualArity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeArityMismatchError"),
              Core.projectionFieldName = (Core.Name "actualArity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.TypeMismatchError
typeMismatchError :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Checking.TypeMismatchError
typeMismatchError expectedType actualType =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Typed.unTypedTerm expectedType)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Typed.unTypedTerm actualType)}]}))
-- | DSL accessor for the actualType field of hydra.error.checking.TypeMismatchError
typeMismatchErrorActualType :: Typed.TypedTerm Checking.TypeMismatchError -> Typed.TypedTerm Core.Type
typeMismatchErrorActualType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
        Core.projectionFieldName = (Core.Name "actualType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expectedType field of hydra.error.checking.TypeMismatchError
typeMismatchErrorExpectedType :: Typed.TypedTerm Checking.TypeMismatchError -> Typed.TypedTerm Core.Type
typeMismatchErrorExpectedType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
        Core.projectionFieldName = (Core.Name "expectedType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the actualType field of hydra.error.checking.TypeMismatchError
typeMismatchErrorWithActualType :: Typed.TypedTerm Checking.TypeMismatchError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Checking.TypeMismatchError
typeMismatchErrorWithActualType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
              Core.projectionFieldName = (Core.Name "expectedType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expectedType field of hydra.error.checking.TypeMismatchError
typeMismatchErrorWithExpectedType :: Typed.TypedTerm Checking.TypeMismatchError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Checking.TypeMismatchError
typeMismatchErrorWithExpectedType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expectedType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actualType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.TypeMismatchError"),
              Core.projectionFieldName = (Core.Name "actualType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.checking.UnboundTypeVariablesError
unboundTypeVariablesError :: Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Checking.UnboundTypeVariablesError
unboundTypeVariablesError variables type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Typed.unTypedTerm variables)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the type field of hydra.error.checking.UnboundTypeVariablesError
unboundTypeVariablesErrorType :: Typed.TypedTerm Checking.UnboundTypeVariablesError -> Typed.TypedTerm Core.Type
unboundTypeVariablesErrorType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variables field of hydra.error.checking.UnboundTypeVariablesError
unboundTypeVariablesErrorVariables :: Typed.TypedTerm Checking.UnboundTypeVariablesError -> Typed.TypedTerm (S.Set Core.Name)
unboundTypeVariablesErrorVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
        Core.projectionFieldName = (Core.Name "variables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the type field of hydra.error.checking.UnboundTypeVariablesError
unboundTypeVariablesErrorWithType :: Typed.TypedTerm Checking.UnboundTypeVariablesError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Checking.UnboundTypeVariablesError
unboundTypeVariablesErrorWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
              Core.projectionFieldName = (Core.Name "variables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the variables field of hydra.error.checking.UnboundTypeVariablesError
unboundTypeVariablesErrorWithVariables :: Typed.TypedTerm Checking.UnboundTypeVariablesError -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Checking.UnboundTypeVariablesError
unboundTypeVariablesErrorWithVariables original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variables"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnboundTypeVariablesError"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingError path name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.error.checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorName :: Typed.TypedTerm Checking.UndefinedTermVariableCheckingError -> Typed.TypedTerm Core.Name
undefinedTermVariableCheckingErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the path field of hydra.error.checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorPath :: Typed.TypedTerm Checking.UndefinedTermVariableCheckingError -> Typed.TypedTerm Paths.SubtermPath
undefinedTermVariableCheckingErrorPath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.error.checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorWithName :: Typed.TypedTerm Checking.UndefinedTermVariableCheckingError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the path field of hydra.error.checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorWithPath :: Typed.TypedTerm Checking.UndefinedTermVariableCheckingError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Checking.UndefinedTermVariableCheckingError
undefinedTermVariableCheckingErrorWithPath original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UndefinedTermVariableCheckingError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.checking.UnequalTypesError
unequalTypesError :: Typed.TypedTerm [Core.Type] -> Typed.TypedTerm String -> Typed.TypedTerm Checking.UnequalTypesError
unequalTypesError types description =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Typed.unTypedTerm types)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm description)}]}))
-- | DSL accessor for the description field of hydra.error.checking.UnequalTypesError
unequalTypesErrorDescription :: Typed.TypedTerm Checking.UnequalTypesError -> Typed.TypedTerm String
unequalTypesErrorDescription x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the types field of hydra.error.checking.UnequalTypesError
unequalTypesErrorTypes :: Typed.TypedTerm Checking.UnequalTypesError -> Typed.TypedTerm [Core.Type]
unequalTypesErrorTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
        Core.projectionFieldName = (Core.Name "types")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the description field of hydra.error.checking.UnequalTypesError
unequalTypesErrorWithDescription :: Typed.TypedTerm Checking.UnequalTypesError -> Typed.TypedTerm String -> Typed.TypedTerm Checking.UnequalTypesError
unequalTypesErrorWithDescription original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the types field of hydra.error.checking.UnequalTypesError
unequalTypesErrorWithTypes :: Typed.TypedTerm Checking.UnequalTypesError -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Checking.UnequalTypesError
unequalTypesErrorWithTypes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UnequalTypesError"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.error.checking.UnsupportedTermVariantError
unsupportedTermVariantError :: Typed.TypedTerm Variants.TermVariant -> Typed.TypedTerm Checking.UnsupportedTermVariantError
unsupportedTermVariantError termVariant =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnsupportedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "termVariant"),
          Core.fieldTerm = (Typed.unTypedTerm termVariant)}]}))
-- | DSL accessor for the termVariant field of hydra.error.checking.UnsupportedTermVariantError
unsupportedTermVariantErrorTermVariant :: Typed.TypedTerm Checking.UnsupportedTermVariantError -> Typed.TypedTerm Variants.TermVariant
unsupportedTermVariantErrorTermVariant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UnsupportedTermVariantError"),
        Core.projectionFieldName = (Core.Name "termVariant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the termVariant field of hydra.error.checking.UnsupportedTermVariantError
unsupportedTermVariantErrorWithTermVariant :: Typed.TypedTerm Checking.UnsupportedTermVariantError -> Typed.TypedTerm Variants.TermVariant -> Typed.TypedTerm Checking.UnsupportedTermVariantError
unsupportedTermVariantErrorWithTermVariant original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UnsupportedTermVariantError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "termVariant"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.UntypedLambdaError
untypedLambdaError :: Typed.TypedTerm Checking.UntypedLambdaError
untypedLambdaError =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLambdaError"),
      Core.recordFields = []}))
-- | DSL constructor for hydra.error.checking.UntypedLetBindingError
untypedLetBindingError :: Typed.TypedTerm Core.Binding -> Typed.TypedTerm Checking.UntypedLetBindingError
untypedLetBindingError binding =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLetBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binding"),
          Core.fieldTerm = (Typed.unTypedTerm binding)}]}))
-- | DSL accessor for the binding field of hydra.error.checking.UntypedLetBindingError
untypedLetBindingErrorBinding :: Typed.TypedTerm Checking.UntypedLetBindingError -> Typed.TypedTerm Core.Binding
untypedLetBindingErrorBinding x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedLetBindingError"),
        Core.projectionFieldName = (Core.Name "binding")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binding field of hydra.error.checking.UntypedLetBindingError
untypedLetBindingErrorWithBinding :: Typed.TypedTerm Checking.UntypedLetBindingError -> Typed.TypedTerm Core.Binding -> Typed.TypedTerm Checking.UntypedLetBindingError
untypedLetBindingErrorWithBinding original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedLetBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "binding"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.error.checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingError path name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.error.checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorName :: Typed.TypedTerm Checking.UntypedTermVariableCheckingError -> Typed.TypedTerm Core.Name
untypedTermVariableCheckingErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the path field of hydra.error.checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorPath :: Typed.TypedTerm Checking.UntypedTermVariableCheckingError -> Typed.TypedTerm Paths.SubtermPath
untypedTermVariableCheckingErrorPath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.error.checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorWithName :: Typed.TypedTerm Checking.UntypedTermVariableCheckingError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the path field of hydra.error.checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorWithPath :: Typed.TypedTerm Checking.UntypedTermVariableCheckingError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Checking.UntypedTermVariableCheckingError
untypedTermVariableCheckingErrorWithPath original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.error.checking.UntypedTermVariableCheckingError"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
