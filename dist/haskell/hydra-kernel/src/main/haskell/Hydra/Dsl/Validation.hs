-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.validation

module Hydra.Dsl.Validation where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Typed as Typed
import qualified Hydra.Validation as Validation
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | DSL constructor for hydra.validation.ValidationProfile
validationProfile :: Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Int -> Typed.TypedTerm Int -> Typed.TypedTerm Validation.ValidationProfile
validationProfile errorRules warningRules maxErrors maxWarnings =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = (Typed.unTypedTerm errorRules)},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = (Typed.unTypedTerm warningRules)},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = (Typed.unTypedTerm maxErrors)},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = (Typed.unTypedTerm maxWarnings)}]}))
-- | DSL accessor for the errorRules field of hydra.validation.ValidationProfile
validationProfileErrorRules :: Typed.TypedTerm Validation.ValidationProfile -> Typed.TypedTerm (S.Set Core.Name)
validationProfileErrorRules x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
        Core.projectionFieldName = (Core.Name "errorRules")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the maxErrors field of hydra.validation.ValidationProfile
validationProfileMaxErrors :: Typed.TypedTerm Validation.ValidationProfile -> Typed.TypedTerm Int
validationProfileMaxErrors x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
        Core.projectionFieldName = (Core.Name "maxErrors")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the maxWarnings field of hydra.validation.ValidationProfile
validationProfileMaxWarnings :: Typed.TypedTerm Validation.ValidationProfile -> Typed.TypedTerm Int
validationProfileMaxWarnings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
        Core.projectionFieldName = (Core.Name "maxWarnings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the warningRules field of hydra.validation.ValidationProfile
validationProfileWarningRules :: Typed.TypedTerm Validation.ValidationProfile -> Typed.TypedTerm (S.Set Core.Name)
validationProfileWarningRules x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
        Core.projectionFieldName = (Core.Name "warningRules")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the errorRules field of hydra.validation.ValidationProfile
validationProfileWithErrorRules :: Typed.TypedTerm Validation.ValidationProfile -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Validation.ValidationProfile
validationProfileWithErrorRules original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "warningRules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxErrors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxWarnings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the maxErrors field of hydra.validation.ValidationProfile
validationProfileWithMaxErrors :: Typed.TypedTerm Validation.ValidationProfile -> Typed.TypedTerm Int -> Typed.TypedTerm Validation.ValidationProfile
validationProfileWithMaxErrors original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "errorRules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "warningRules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxWarnings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the maxWarnings field of hydra.validation.ValidationProfile
validationProfileWithMaxWarnings :: Typed.TypedTerm Validation.ValidationProfile -> Typed.TypedTerm Int -> Typed.TypedTerm Validation.ValidationProfile
validationProfileWithMaxWarnings original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "errorRules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "warningRules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxErrors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the warningRules field of hydra.validation.ValidationProfile
validationProfileWithWarningRules :: Typed.TypedTerm Validation.ValidationProfile -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Validation.ValidationProfile
validationProfileWithWarningRules original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "errorRules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxErrors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxWarnings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.validation.ValidationResult
validationResult :: Typed.TypedTerm [e] -> Typed.TypedTerm [e] -> Typed.TypedTerm (Validation.ValidationResult e)
validationResult errors warnings =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errors"),
          Core.fieldTerm = (Typed.unTypedTerm errors)},
        Core.Field {
          Core.fieldName = (Core.Name "warnings"),
          Core.fieldTerm = (Typed.unTypedTerm warnings)}]}))
-- | DSL accessor for the errors field of hydra.validation.ValidationResult
validationResultErrors :: Typed.TypedTerm (Validation.ValidationResult e) -> Typed.TypedTerm [e]
validationResultErrors x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationResult"),
        Core.projectionFieldName = (Core.Name "errors")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the warnings field of hydra.validation.ValidationResult
validationResultWarnings :: Typed.TypedTerm (Validation.ValidationResult e) -> Typed.TypedTerm [e]
validationResultWarnings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationResult"),
        Core.projectionFieldName = (Core.Name "warnings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the errors field of hydra.validation.ValidationResult
validationResultWithErrors :: Typed.TypedTerm (Validation.ValidationResult e) -> Typed.TypedTerm [e] -> Typed.TypedTerm (Validation.ValidationResult e)
validationResultWithErrors original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errors"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "warnings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationResult"),
              Core.projectionFieldName = (Core.Name "warnings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the warnings field of hydra.validation.ValidationResult
validationResultWithWarnings :: Typed.TypedTerm (Validation.ValidationResult e) -> Typed.TypedTerm [e] -> Typed.TypedTerm (Validation.ValidationResult e)
validationResultWithWarnings original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationResult"),
              Core.projectionFieldName = (Core.Name "errors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "warnings"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
