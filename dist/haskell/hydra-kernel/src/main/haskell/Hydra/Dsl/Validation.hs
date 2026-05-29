-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.validation

module Hydra.Dsl.Validation where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Validation as Validation
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | DSL constructor for hydra.validation.ValidationProfile
validationProfile :: Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm Validation.ValidationProfile
validationProfile errorRules warningRules maxErrors maxWarnings =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = (Phantoms.unTTerm errorRules)},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = (Phantoms.unTTerm warningRules)},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = (Phantoms.unTTerm maxErrors)},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = (Phantoms.unTTerm maxWarnings)}]}))
-- | DSL accessor for the errorRules field of hydra.validation.ValidationProfile
validationProfileErrorRules :: Phantoms.TTerm Validation.ValidationProfile -> Phantoms.TTerm (S.Set Core.Name)
validationProfileErrorRules x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
        Core.projectionFieldName = (Core.Name "errorRules")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the maxErrors field of hydra.validation.ValidationProfile
validationProfileMaxErrors :: Phantoms.TTerm Validation.ValidationProfile -> Phantoms.TTerm Int
validationProfileMaxErrors x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
        Core.projectionFieldName = (Core.Name "maxErrors")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the maxWarnings field of hydra.validation.ValidationProfile
validationProfileMaxWarnings :: Phantoms.TTerm Validation.ValidationProfile -> Phantoms.TTerm Int
validationProfileMaxWarnings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
        Core.projectionFieldName = (Core.Name "maxWarnings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the warningRules field of hydra.validation.ValidationProfile
validationProfileWarningRules :: Phantoms.TTerm Validation.ValidationProfile -> Phantoms.TTerm (S.Set Core.Name)
validationProfileWarningRules x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
        Core.projectionFieldName = (Core.Name "warningRules")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the errorRules field of hydra.validation.ValidationProfile
validationProfileWithErrorRules :: Phantoms.TTerm Validation.ValidationProfile -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Validation.ValidationProfile
validationProfileWithErrorRules original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "warningRules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxErrors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxWarnings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the maxErrors field of hydra.validation.ValidationProfile
validationProfileWithMaxErrors :: Phantoms.TTerm Validation.ValidationProfile -> Phantoms.TTerm Int -> Phantoms.TTerm Validation.ValidationProfile
validationProfileWithMaxErrors original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "errorRules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "warningRules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxWarnings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the maxWarnings field of hydra.validation.ValidationProfile
validationProfileWithMaxWarnings :: Phantoms.TTerm Validation.ValidationProfile -> Phantoms.TTerm Int -> Phantoms.TTerm Validation.ValidationProfile
validationProfileWithMaxWarnings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "errorRules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "warningRules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxErrors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the warningRules field of hydra.validation.ValidationProfile
validationProfileWithWarningRules :: Phantoms.TTerm Validation.ValidationProfile -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Validation.ValidationProfile
validationProfileWithWarningRules original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationProfile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errorRules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "errorRules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "warningRules"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "maxErrors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxErrors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxWarnings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationProfile"),
              Core.projectionFieldName = (Core.Name "maxWarnings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.validation.ValidationResult
validationResult :: Phantoms.TTerm [e] -> Phantoms.TTerm [e] -> Phantoms.TTerm (Validation.ValidationResult e)
validationResult errors warnings =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errors"),
          Core.fieldTerm = (Phantoms.unTTerm errors)},
        Core.Field {
          Core.fieldName = (Core.Name "warnings"),
          Core.fieldTerm = (Phantoms.unTTerm warnings)}]}))
-- | DSL accessor for the errors field of hydra.validation.ValidationResult
validationResultErrors :: Phantoms.TTerm (Validation.ValidationResult e) -> Phantoms.TTerm [e]
validationResultErrors x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationResult"),
        Core.projectionFieldName = (Core.Name "errors")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the warnings field of hydra.validation.ValidationResult
validationResultWarnings :: Phantoms.TTerm (Validation.ValidationResult e) -> Phantoms.TTerm [e]
validationResultWarnings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.validation.ValidationResult"),
        Core.projectionFieldName = (Core.Name "warnings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the errors field of hydra.validation.ValidationResult
validationResultWithErrors :: Phantoms.TTerm (Validation.ValidationResult e) -> Phantoms.TTerm [e] -> Phantoms.TTerm (Validation.ValidationResult e)
validationResultWithErrors original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errors"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "warnings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationResult"),
              Core.projectionFieldName = (Core.Name "warnings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the warnings field of hydra.validation.ValidationResult
validationResultWithWarnings :: Phantoms.TTerm (Validation.ValidationResult e) -> Phantoms.TTerm [e] -> Phantoms.TTerm (Validation.ValidationResult e)
validationResultWithWarnings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.validation.ValidationResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "errors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.validation.ValidationResult"),
              Core.projectionFieldName = (Core.Name "errors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "warnings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
