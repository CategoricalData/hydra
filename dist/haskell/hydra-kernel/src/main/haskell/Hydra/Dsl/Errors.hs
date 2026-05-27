-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.errors

module Hydra.Dsl.Errors where
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Context as DslContext
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Typing as Typing
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for the hydra.errors.DecodingError wrapper
decodingError :: Phantoms.TTerm String -> Phantoms.TTerm Errors.DecodingError
decodingError x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the checking variant of hydra.errors.Error
errorChecking :: Phantoms.TTerm Checking.CheckingError -> Phantoms.TTerm Errors.Error
errorChecking x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "checking"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the decoding variant of hydra.errors.Error
errorDecoding :: Phantoms.TTerm Errors.DecodingError -> Phantoms.TTerm Errors.Error
errorDecoding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decoding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the duplicateBinding variant of hydra.errors.Error
errorDuplicateBinding :: Phantoms.TTerm ErrorCore.DuplicateBindingError -> Phantoms.TTerm Errors.Error
errorDuplicateBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the duplicateField variant of hydra.errors.Error
errorDuplicateField :: Phantoms.TTerm ErrorCore.DuplicateFieldError -> Phantoms.TTerm Errors.Error
errorDuplicateField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the extraction variant of hydra.errors.Error
errorExtraction :: Phantoms.TTerm Errors.ExtractionError -> Phantoms.TTerm Errors.Error
errorExtraction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extraction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the inference variant of hydra.errors.Error
errorInference :: Phantoms.TTerm Errors.InferenceError -> Phantoms.TTerm Errors.Error
errorInference x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the invalidLiteral variant of hydra.errors.Error
errorInvalidLiteral :: Phantoms.TTerm ErrorCore.InvalidLiteralError -> Phantoms.TTerm Errors.Error
errorInvalidLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidLiteral"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the other variant of hydra.errors.Error
errorOther :: Phantoms.TTerm Errors.OtherError -> Phantoms.TTerm Errors.Error
errorOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the resolution variant of hydra.errors.Error
errorResolution :: Phantoms.TTerm Errors.ResolutionError -> Phantoms.TTerm Errors.Error
errorResolution x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "resolution"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the undefinedField variant of hydra.errors.Error
errorUndefinedField :: Phantoms.TTerm ErrorCore.UndefinedFieldError -> Phantoms.TTerm Errors.Error
errorUndefinedField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the undefinedTermVariable variant of hydra.errors.Error
errorUndefinedTermVariable :: Phantoms.TTerm ErrorCore.UndefinedTermVariableError -> Phantoms.TTerm Errors.Error
errorUndefinedTermVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unexpectedTermVariant variant of hydra.errors.Error
errorUnexpectedTermVariant :: Phantoms.TTerm ErrorCore.UnexpectedTermVariantError -> Phantoms.TTerm Errors.Error
errorUnexpectedTermVariant x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedTermVariant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unexpectedTypeVariant variant of hydra.errors.Error
errorUnexpectedTypeVariant :: Phantoms.TTerm ErrorCore.UnexpectedTypeVariantError -> Phantoms.TTerm Errors.Error
errorUnexpectedTypeVariant x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedTypeVariant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unification variant of hydra.errors.Error
errorUnification :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Errors.Error
errorUnification x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unification"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the untypedTermVariable variant of hydra.errors.Error
errorUntypedTermVariable :: Phantoms.TTerm ErrorCore.UntypedTermVariableError -> Phantoms.TTerm Errors.Error
errorUntypedTermVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the emptyList variant of hydra.errors.ExtractionError
extractionErrorEmptyList :: Phantoms.TTerm Errors.EmptyListError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorEmptyList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyList"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the multipleBindings variant of hydra.errors.ExtractionError
extractionErrorMultipleBindings :: Phantoms.TTerm Errors.MultipleBindingsError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorMultipleBindings x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multipleBindings"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the multipleFields variant of hydra.errors.ExtractionError
extractionErrorMultipleFields :: Phantoms.TTerm Errors.MultipleFieldsError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorMultipleFields x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multipleFields"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the noMatchingField variant of hydra.errors.ExtractionError
extractionErrorNoMatchingField :: Phantoms.TTerm Errors.NoMatchingFieldError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorNoMatchingField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noMatchingField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the noSuchBinding variant of hydra.errors.ExtractionError
extractionErrorNoSuchBinding :: Phantoms.TTerm Errors.NoSuchBindingError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorNoSuchBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noSuchBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the notEnoughCases variant of hydra.errors.ExtractionError
extractionErrorNotEnoughCases :: Phantoms.TTerm Errors.NotEnoughCasesError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorNotEnoughCases x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEnoughCases"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unexpectedShape variant of hydra.errors.ExtractionError
extractionErrorUnexpectedShape :: Phantoms.TTerm Errors.UnexpectedShapeError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorUnexpectedShape x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedShape"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the checking variant of hydra.errors.InferenceError
inferenceErrorChecking :: Phantoms.TTerm Checking.CheckingError -> Phantoms.TTerm Errors.InferenceError
inferenceErrorChecking x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "checking"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the other variant of hydra.errors.InferenceError
inferenceErrorOther :: Phantoms.TTerm Errors.OtherInferenceError -> Phantoms.TTerm Errors.InferenceError
inferenceErrorOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unification variant of hydra.errors.InferenceError
inferenceErrorUnification :: Phantoms.TTerm Errors.UnificationInferenceError -> Phantoms.TTerm Errors.InferenceError
inferenceErrorUnification x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unification"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.errors.MultipleBindingsError
multipleBindingsError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.MultipleBindingsError
multipleBindingsError name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.errors.MultipleBindingsError
multipleBindingsErrorName :: Phantoms.TTerm Errors.MultipleBindingsError -> Phantoms.TTerm Core.Name
multipleBindingsErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.errors.MultipleBindingsError
multipleBindingsErrorWithName :: Phantoms.TTerm Errors.MultipleBindingsError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.MultipleBindingsError
multipleBindingsErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.errors.MultipleFieldsError
multipleFieldsError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.MultipleFieldsError
multipleFieldsError fieldName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm fieldName)}]}))
-- | DSL accessor for the fieldName field of hydra.errors.MultipleFieldsError
multipleFieldsErrorFieldName :: Phantoms.TTerm Errors.MultipleFieldsError -> Phantoms.TTerm Core.Name
multipleFieldsErrorFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
        Core.projectionFieldName = (Core.Name "fieldName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the fieldName field of hydra.errors.MultipleFieldsError
multipleFieldsErrorWithFieldName :: Phantoms.TTerm Errors.MultipleFieldsError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.MultipleFieldsError
multipleFieldsErrorWithFieldName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.errors.NoMatchingFieldError
noMatchingFieldError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoMatchingFieldError
noMatchingFieldError fieldName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm fieldName)}]}))
-- | DSL accessor for the fieldName field of hydra.errors.NoMatchingFieldError
noMatchingFieldErrorFieldName :: Phantoms.TTerm Errors.NoMatchingFieldError -> Phantoms.TTerm Core.Name
noMatchingFieldErrorFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
        Core.projectionFieldName = (Core.Name "fieldName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the fieldName field of hydra.errors.NoMatchingFieldError
noMatchingFieldErrorWithFieldName :: Phantoms.TTerm Errors.NoMatchingFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoMatchingFieldError
noMatchingFieldErrorWithFieldName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.errors.NoSuchBindingError
noSuchBindingError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoSuchBindingError
noSuchBindingError name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.errors.NoSuchBindingError
noSuchBindingErrorName :: Phantoms.TTerm Errors.NoSuchBindingError -> Phantoms.TTerm Core.Name
noSuchBindingErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.errors.NoSuchBindingError
noSuchBindingErrorWithName :: Phantoms.TTerm Errors.NoSuchBindingError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoSuchBindingError
noSuchBindingErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.errors.NoSuchPrimitiveError
noSuchPrimitiveError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoSuchPrimitiveError
noSuchPrimitiveError name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.errors.NoSuchPrimitiveError
noSuchPrimitiveErrorName :: Phantoms.TTerm Errors.NoSuchPrimitiveError -> Phantoms.TTerm Core.Name
noSuchPrimitiveErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.errors.NoSuchPrimitiveError
noSuchPrimitiveErrorWithName :: Phantoms.TTerm Errors.NoSuchPrimitiveError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoSuchPrimitiveError
noSuchPrimitiveErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.errors.OtherError wrapper
otherError :: Phantoms.TTerm String -> Phantoms.TTerm Errors.OtherError
otherError x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherError"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.errors.OtherInferenceError
otherInferenceError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm String -> Phantoms.TTerm Errors.OtherInferenceError
otherInferenceError path message =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm message)}]}))
-- | DSL accessor for the message field of hydra.errors.OtherInferenceError
otherInferenceErrorMessage :: Phantoms.TTerm Errors.OtherInferenceError -> Phantoms.TTerm String
otherInferenceErrorMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
        Core.projectionFieldName = (Core.Name "message")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the path field of hydra.errors.OtherInferenceError
otherInferenceErrorPath :: Phantoms.TTerm Errors.OtherInferenceError -> Phantoms.TTerm Paths.SubtermPath
otherInferenceErrorPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the message field of hydra.errors.OtherInferenceError
otherInferenceErrorWithMessage :: Phantoms.TTerm Errors.OtherInferenceError -> Phantoms.TTerm String -> Phantoms.TTerm Errors.OtherInferenceError
otherInferenceErrorWithMessage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the path field of hydra.errors.OtherInferenceError
otherInferenceErrorWithPath :: Phantoms.TTerm Errors.OtherInferenceError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Errors.OtherInferenceError
otherInferenceErrorWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.errors.OtherResolutionError wrapper
otherResolutionError :: Phantoms.TTerm String -> Phantoms.TTerm Errors.OtherResolutionError
otherResolutionError x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherResolutionError"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the noMatchingField variant of hydra.errors.ResolutionError
resolutionErrorNoMatchingField :: Phantoms.TTerm Errors.NoMatchingFieldError -> Phantoms.TTerm Errors.ResolutionError
resolutionErrorNoMatchingField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noMatchingField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the noSuchBinding variant of hydra.errors.ResolutionError
resolutionErrorNoSuchBinding :: Phantoms.TTerm Errors.NoSuchBindingError -> Phantoms.TTerm Errors.ResolutionError
resolutionErrorNoSuchBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noSuchBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the noSuchPrimitive variant of hydra.errors.ResolutionError
resolutionErrorNoSuchPrimitive :: Phantoms.TTerm Errors.NoSuchPrimitiveError -> Phantoms.TTerm Errors.ResolutionError
resolutionErrorNoSuchPrimitive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noSuchPrimitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the other variant of hydra.errors.ResolutionError
resolutionErrorOther :: Phantoms.TTerm Errors.OtherResolutionError -> Phantoms.TTerm Errors.ResolutionError
resolutionErrorOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unexpectedShape variant of hydra.errors.ResolutionError
resolutionErrorUnexpectedShape :: Phantoms.TTerm Errors.UnexpectedShapeError -> Phantoms.TTerm Errors.ResolutionError
resolutionErrorUnexpectedShape x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedShape"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL accessor for the body of hydra.errors.DecodingError
unDecodingError :: Phantoms.TTerm Errors.DecodingError -> Phantoms.TTerm String
unDecodingError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.errors.DecodingError")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.errors.OtherError
unOtherError :: Phantoms.TTerm Errors.OtherError -> Phantoms.TTerm String
unOtherError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.errors.OtherError")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.errors.OtherResolutionError
unOtherResolutionError :: Phantoms.TTerm Errors.OtherResolutionError -> Phantoms.TTerm String
unOtherResolutionError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.errors.OtherResolutionError")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.errors.UnexpectedShapeError
unexpectedShapeError :: Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Errors.UnexpectedShapeError
unexpectedShapeError expected actual =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Phantoms.unTTerm actual)}]}))
-- | DSL accessor for the actual field of hydra.errors.UnexpectedShapeError
unexpectedShapeErrorActual :: Phantoms.TTerm Errors.UnexpectedShapeError -> Phantoms.TTerm String
unexpectedShapeErrorActual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
        Core.projectionFieldName = (Core.Name "actual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expected field of hydra.errors.UnexpectedShapeError
unexpectedShapeErrorExpected :: Phantoms.TTerm Errors.UnexpectedShapeError -> Phantoms.TTerm String
unexpectedShapeErrorExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
        Core.projectionFieldName = (Core.Name "expected")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the actual field of hydra.errors.UnexpectedShapeError
unexpectedShapeErrorWithActual :: Phantoms.TTerm Errors.UnexpectedShapeError -> Phantoms.TTerm String -> Phantoms.TTerm Errors.UnexpectedShapeError
unexpectedShapeErrorWithActual original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
              Core.projectionFieldName = (Core.Name "expected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expected field of hydra.errors.UnexpectedShapeError
unexpectedShapeErrorWithExpected :: Phantoms.TTerm Errors.UnexpectedShapeError -> Phantoms.TTerm String -> Phantoms.TTerm Errors.UnexpectedShapeError
unexpectedShapeErrorWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
              Core.projectionFieldName = (Core.Name "actual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.errors.UnificationError
unificationError :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm String -> Phantoms.TTerm Errors.UnificationError
unificationError leftType rightType message =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Phantoms.unTTerm leftType)},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Phantoms.unTTerm rightType)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm message)}]}))
-- | DSL accessor for the leftType field of hydra.errors.UnificationError
unificationErrorLeftType :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Core.Type
unificationErrorLeftType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionFieldName = (Core.Name "leftType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the message field of hydra.errors.UnificationError
unificationErrorMessage :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm String
unificationErrorMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionFieldName = (Core.Name "message")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rightType field of hydra.errors.UnificationError
unificationErrorRightType :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Core.Type
unificationErrorRightType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionFieldName = (Core.Name "rightType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the leftType field of hydra.errors.UnificationError
unificationErrorWithLeftType :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Errors.UnificationError
unificationErrorWithLeftType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "rightType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the message field of hydra.errors.UnificationError
unificationErrorWithMessage :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm String -> Phantoms.TTerm Errors.UnificationError
unificationErrorWithMessage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "leftType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "rightType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the rightType field of hydra.errors.UnificationError
unificationErrorWithRightType :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Errors.UnificationError
unificationErrorWithRightType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "leftType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.errors.UnificationInferenceError
unificationInferenceError :: Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Errors.UnificationInferenceError
unificationInferenceError path cause =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "cause"),
          Core.fieldTerm = (Phantoms.unTTerm cause)}]}))
-- | DSL accessor for the cause field of hydra.errors.UnificationInferenceError
unificationInferenceErrorCause :: Phantoms.TTerm Errors.UnificationInferenceError -> Phantoms.TTerm Errors.UnificationError
unificationInferenceErrorCause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
        Core.projectionFieldName = (Core.Name "cause")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the path field of hydra.errors.UnificationInferenceError
unificationInferenceErrorPath :: Phantoms.TTerm Errors.UnificationInferenceError -> Phantoms.TTerm Paths.SubtermPath
unificationInferenceErrorPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cause field of hydra.errors.UnificationInferenceError
unificationInferenceErrorWithCause :: Phantoms.TTerm Errors.UnificationInferenceError -> Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Errors.UnificationInferenceError
unificationInferenceErrorWithCause original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cause"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the path field of hydra.errors.UnificationInferenceError
unificationInferenceErrorWithPath :: Phantoms.TTerm Errors.UnificationInferenceError -> Phantoms.TTerm Paths.SubtermPath -> Phantoms.TTerm Errors.UnificationInferenceError
unificationInferenceErrorWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
              Core.projectionFieldName = (Core.Name "cause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
