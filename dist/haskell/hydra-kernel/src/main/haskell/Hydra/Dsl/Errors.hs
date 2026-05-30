-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.errors

module Hydra.Dsl.Errors where
import qualified Hydra.Core as Core
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
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for the hydra.errors.DecodingError wrapper
decodingError :: Typed.TypedTerm String -> Typed.TypedTerm Errors.DecodingError
decodingError x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the checking variant of hydra.errors.Error
errorChecking :: Typed.TypedTerm Checking.CheckingError -> Typed.TypedTerm Errors.Error
errorChecking x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "checking"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the decoding variant of hydra.errors.Error
errorDecoding :: Typed.TypedTerm Errors.DecodingError -> Typed.TypedTerm Errors.Error
errorDecoding x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decoding"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the duplicateBinding variant of hydra.errors.Error
errorDuplicateBinding :: Typed.TypedTerm ErrorCore.DuplicateBindingError -> Typed.TypedTerm Errors.Error
errorDuplicateBinding x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateBinding"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the duplicateField variant of hydra.errors.Error
errorDuplicateField :: Typed.TypedTerm ErrorCore.DuplicateFieldError -> Typed.TypedTerm Errors.Error
errorDuplicateField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateField"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the extraction variant of hydra.errors.Error
errorExtraction :: Typed.TypedTerm Errors.ExtractionError -> Typed.TypedTerm Errors.Error
errorExtraction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extraction"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the inference variant of hydra.errors.Error
errorInference :: Typed.TypedTerm Errors.InferenceError -> Typed.TypedTerm Errors.Error
errorInference x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inference"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the invalidLiteral variant of hydra.errors.Error
errorInvalidLiteral :: Typed.TypedTerm ErrorCore.InvalidLiteralError -> Typed.TypedTerm Errors.Error
errorInvalidLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "invalidLiteral"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the other variant of hydra.errors.Error
errorOther :: Typed.TypedTerm Errors.OtherError -> Typed.TypedTerm Errors.Error
errorOther x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the resolution variant of hydra.errors.Error
errorResolution :: Typed.TypedTerm Errors.ResolutionError -> Typed.TypedTerm Errors.Error
errorResolution x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "resolution"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the undefinedField variant of hydra.errors.Error
errorUndefinedField :: Typed.TypedTerm ErrorCore.UndefinedFieldError -> Typed.TypedTerm Errors.Error
errorUndefinedField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedField"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the undefinedTermVariable variant of hydra.errors.Error
errorUndefinedTermVariable :: Typed.TypedTerm ErrorCore.UndefinedTermVariableError -> Typed.TypedTerm Errors.Error
errorUndefinedTermVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTermVariable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unexpectedTermVariant variant of hydra.errors.Error
errorUnexpectedTermVariant :: Typed.TypedTerm ErrorCore.UnexpectedTermVariantError -> Typed.TypedTerm Errors.Error
errorUnexpectedTermVariant x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedTermVariant"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unexpectedTypeVariant variant of hydra.errors.Error
errorUnexpectedTypeVariant :: Typed.TypedTerm ErrorCore.UnexpectedTypeVariantError -> Typed.TypedTerm Errors.Error
errorUnexpectedTypeVariant x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedTypeVariant"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unification variant of hydra.errors.Error
errorUnification :: Typed.TypedTerm Errors.UnificationError -> Typed.TypedTerm Errors.Error
errorUnification x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unification"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the untypedTermVariable variant of hydra.errors.Error
errorUntypedTermVariable :: Typed.TypedTerm ErrorCore.UntypedTermVariableError -> Typed.TypedTerm Errors.Error
errorUntypedTermVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedTermVariable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the emptyList variant of hydra.errors.ExtractionError
extractionErrorEmptyList :: Typed.TypedTerm Errors.EmptyListError -> Typed.TypedTerm Errors.ExtractionError
extractionErrorEmptyList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyList"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the multipleBindings variant of hydra.errors.ExtractionError
extractionErrorMultipleBindings :: Typed.TypedTerm Errors.MultipleBindingsError -> Typed.TypedTerm Errors.ExtractionError
extractionErrorMultipleBindings x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multipleBindings"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the multipleFields variant of hydra.errors.ExtractionError
extractionErrorMultipleFields :: Typed.TypedTerm Errors.MultipleFieldsError -> Typed.TypedTerm Errors.ExtractionError
extractionErrorMultipleFields x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multipleFields"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the noMatchingField variant of hydra.errors.ExtractionError
extractionErrorNoMatchingField :: Typed.TypedTerm Errors.NoMatchingFieldError -> Typed.TypedTerm Errors.ExtractionError
extractionErrorNoMatchingField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noMatchingField"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the noSuchBinding variant of hydra.errors.ExtractionError
extractionErrorNoSuchBinding :: Typed.TypedTerm Errors.NoSuchBindingError -> Typed.TypedTerm Errors.ExtractionError
extractionErrorNoSuchBinding x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noSuchBinding"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the notEnoughCases variant of hydra.errors.ExtractionError
extractionErrorNotEnoughCases :: Typed.TypedTerm Errors.NotEnoughCasesError -> Typed.TypedTerm Errors.ExtractionError
extractionErrorNotEnoughCases x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEnoughCases"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unexpectedShape variant of hydra.errors.ExtractionError
extractionErrorUnexpectedShape :: Typed.TypedTerm Errors.UnexpectedShapeError -> Typed.TypedTerm Errors.ExtractionError
extractionErrorUnexpectedShape x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedShape"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the checking variant of hydra.errors.InferenceError
inferenceErrorChecking :: Typed.TypedTerm Checking.CheckingError -> Typed.TypedTerm Errors.InferenceError
inferenceErrorChecking x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "checking"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the other variant of hydra.errors.InferenceError
inferenceErrorOther :: Typed.TypedTerm Errors.OtherInferenceError -> Typed.TypedTerm Errors.InferenceError
inferenceErrorOther x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unification variant of hydra.errors.InferenceError
inferenceErrorUnification :: Typed.TypedTerm Errors.UnificationInferenceError -> Typed.TypedTerm Errors.InferenceError
inferenceErrorUnification x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unification"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.errors.MultipleBindingsError
multipleBindingsError :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Errors.MultipleBindingsError
multipleBindingsError name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.errors.MultipleBindingsError
multipleBindingsErrorName :: Typed.TypedTerm Errors.MultipleBindingsError -> Typed.TypedTerm Core.Name
multipleBindingsErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.errors.MultipleBindingsError
multipleBindingsErrorWithName :: Typed.TypedTerm Errors.MultipleBindingsError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Errors.MultipleBindingsError
multipleBindingsErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.errors.MultipleFieldsError
multipleFieldsError :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Errors.MultipleFieldsError
multipleFieldsError fieldName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Typed.unTypedTerm fieldName)}]}))
-- | DSL accessor for the fieldName field of hydra.errors.MultipleFieldsError
multipleFieldsErrorFieldName :: Typed.TypedTerm Errors.MultipleFieldsError -> Typed.TypedTerm Core.Name
multipleFieldsErrorFieldName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
        Core.projectionFieldName = (Core.Name "fieldName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fieldName field of hydra.errors.MultipleFieldsError
multipleFieldsErrorWithFieldName :: Typed.TypedTerm Errors.MultipleFieldsError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Errors.MultipleFieldsError
multipleFieldsErrorWithFieldName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.errors.NoMatchingFieldError
noMatchingFieldError :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Errors.NoMatchingFieldError
noMatchingFieldError fieldName =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Typed.unTypedTerm fieldName)}]}))
-- | DSL accessor for the fieldName field of hydra.errors.NoMatchingFieldError
noMatchingFieldErrorFieldName :: Typed.TypedTerm Errors.NoMatchingFieldError -> Typed.TypedTerm Core.Name
noMatchingFieldErrorFieldName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
        Core.projectionFieldName = (Core.Name "fieldName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fieldName field of hydra.errors.NoMatchingFieldError
noMatchingFieldErrorWithFieldName :: Typed.TypedTerm Errors.NoMatchingFieldError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Errors.NoMatchingFieldError
noMatchingFieldErrorWithFieldName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.errors.NoSuchBindingError
noSuchBindingError :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Errors.NoSuchBindingError
noSuchBindingError name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.errors.NoSuchBindingError
noSuchBindingErrorName :: Typed.TypedTerm Errors.NoSuchBindingError -> Typed.TypedTerm Core.Name
noSuchBindingErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.errors.NoSuchBindingError
noSuchBindingErrorWithName :: Typed.TypedTerm Errors.NoSuchBindingError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Errors.NoSuchBindingError
noSuchBindingErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.errors.NoSuchPrimitiveError
noSuchPrimitiveError :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Errors.NoSuchPrimitiveError
noSuchPrimitiveError name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.errors.NoSuchPrimitiveError
noSuchPrimitiveErrorName :: Typed.TypedTerm Errors.NoSuchPrimitiveError -> Typed.TypedTerm Core.Name
noSuchPrimitiveErrorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.errors.NoSuchPrimitiveError
noSuchPrimitiveErrorWithName :: Typed.TypedTerm Errors.NoSuchPrimitiveError -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Errors.NoSuchPrimitiveError
noSuchPrimitiveErrorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.errors.OtherError wrapper
otherError :: Typed.TypedTerm String -> Typed.TypedTerm Errors.OtherError
otherError x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherError"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.errors.OtherInferenceError
otherInferenceError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm String -> Typed.TypedTerm Errors.OtherInferenceError
otherInferenceError path message =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Typed.unTypedTerm message)}]}))
-- | DSL accessor for the message field of hydra.errors.OtherInferenceError
otherInferenceErrorMessage :: Typed.TypedTerm Errors.OtherInferenceError -> Typed.TypedTerm String
otherInferenceErrorMessage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
        Core.projectionFieldName = (Core.Name "message")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the path field of hydra.errors.OtherInferenceError
otherInferenceErrorPath :: Typed.TypedTerm Errors.OtherInferenceError -> Typed.TypedTerm Paths.SubtermPath
otherInferenceErrorPath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the message field of hydra.errors.OtherInferenceError
otherInferenceErrorWithMessage :: Typed.TypedTerm Errors.OtherInferenceError -> Typed.TypedTerm String -> Typed.TypedTerm Errors.OtherInferenceError
otherInferenceErrorWithMessage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the path field of hydra.errors.OtherInferenceError
otherInferenceErrorWithPath :: Typed.TypedTerm Errors.OtherInferenceError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Errors.OtherInferenceError
otherInferenceErrorWithPath original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.errors.OtherResolutionError wrapper
otherResolutionError :: Typed.TypedTerm String -> Typed.TypedTerm Errors.OtherResolutionError
otherResolutionError x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherResolutionError"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the noMatchingField variant of hydra.errors.ResolutionError
resolutionErrorNoMatchingField :: Typed.TypedTerm Errors.NoMatchingFieldError -> Typed.TypedTerm Errors.ResolutionError
resolutionErrorNoMatchingField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noMatchingField"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the noSuchBinding variant of hydra.errors.ResolutionError
resolutionErrorNoSuchBinding :: Typed.TypedTerm Errors.NoSuchBindingError -> Typed.TypedTerm Errors.ResolutionError
resolutionErrorNoSuchBinding x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noSuchBinding"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the noSuchPrimitive variant of hydra.errors.ResolutionError
resolutionErrorNoSuchPrimitive :: Typed.TypedTerm Errors.NoSuchPrimitiveError -> Typed.TypedTerm Errors.ResolutionError
resolutionErrorNoSuchPrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noSuchPrimitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the other variant of hydra.errors.ResolutionError
resolutionErrorOther :: Typed.TypedTerm Errors.OtherResolutionError -> Typed.TypedTerm Errors.ResolutionError
resolutionErrorOther x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unexpectedShape variant of hydra.errors.ResolutionError
resolutionErrorUnexpectedShape :: Typed.TypedTerm Errors.UnexpectedShapeError -> Typed.TypedTerm Errors.ResolutionError
resolutionErrorUnexpectedShape x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedShape"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the body of hydra.errors.DecodingError
unDecodingError :: Typed.TypedTerm Errors.DecodingError -> Typed.TypedTerm String
unDecodingError x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.errors.DecodingError")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.errors.OtherError
unOtherError :: Typed.TypedTerm Errors.OtherError -> Typed.TypedTerm String
unOtherError x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.errors.OtherError")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.errors.OtherResolutionError
unOtherResolutionError :: Typed.TypedTerm Errors.OtherResolutionError -> Typed.TypedTerm String
unOtherResolutionError x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.errors.OtherResolutionError")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.errors.UnexpectedShapeError
unexpectedShapeError :: Typed.TypedTerm String -> Typed.TypedTerm String -> Typed.TypedTerm Errors.UnexpectedShapeError
unexpectedShapeError expected actual =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Typed.unTypedTerm expected)},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Typed.unTypedTerm actual)}]}))
-- | DSL accessor for the actual field of hydra.errors.UnexpectedShapeError
unexpectedShapeErrorActual :: Typed.TypedTerm Errors.UnexpectedShapeError -> Typed.TypedTerm String
unexpectedShapeErrorActual x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
        Core.projectionFieldName = (Core.Name "actual")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expected field of hydra.errors.UnexpectedShapeError
unexpectedShapeErrorExpected :: Typed.TypedTerm Errors.UnexpectedShapeError -> Typed.TypedTerm String
unexpectedShapeErrorExpected x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
        Core.projectionFieldName = (Core.Name "expected")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the actual field of hydra.errors.UnexpectedShapeError
unexpectedShapeErrorWithActual :: Typed.TypedTerm Errors.UnexpectedShapeError -> Typed.TypedTerm String -> Typed.TypedTerm Errors.UnexpectedShapeError
unexpectedShapeErrorWithActual original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
              Core.projectionFieldName = (Core.Name "expected")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expected field of hydra.errors.UnexpectedShapeError
unexpectedShapeErrorWithExpected :: Typed.TypedTerm Errors.UnexpectedShapeError -> Typed.TypedTerm String -> Typed.TypedTerm Errors.UnexpectedShapeError
unexpectedShapeErrorWithExpected original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
              Core.projectionFieldName = (Core.Name "actual")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.errors.UnificationError
unificationError :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type -> Typed.TypedTerm String -> Typed.TypedTerm Errors.UnificationError
unificationError leftType rightType message =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Typed.unTypedTerm leftType)},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Typed.unTypedTerm rightType)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Typed.unTypedTerm message)}]}))
-- | DSL accessor for the leftType field of hydra.errors.UnificationError
unificationErrorLeftType :: Typed.TypedTerm Errors.UnificationError -> Typed.TypedTerm Core.Type
unificationErrorLeftType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionFieldName = (Core.Name "leftType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the message field of hydra.errors.UnificationError
unificationErrorMessage :: Typed.TypedTerm Errors.UnificationError -> Typed.TypedTerm String
unificationErrorMessage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionFieldName = (Core.Name "message")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rightType field of hydra.errors.UnificationError
unificationErrorRightType :: Typed.TypedTerm Errors.UnificationError -> Typed.TypedTerm Core.Type
unificationErrorRightType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionFieldName = (Core.Name "rightType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the leftType field of hydra.errors.UnificationError
unificationErrorWithLeftType :: Typed.TypedTerm Errors.UnificationError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Errors.UnificationError
unificationErrorWithLeftType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "rightType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the message field of hydra.errors.UnificationError
unificationErrorWithMessage :: Typed.TypedTerm Errors.UnificationError -> Typed.TypedTerm String -> Typed.TypedTerm Errors.UnificationError
unificationErrorWithMessage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "leftType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "rightType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the rightType field of hydra.errors.UnificationError
unificationErrorWithRightType :: Typed.TypedTerm Errors.UnificationError -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Errors.UnificationError
unificationErrorWithRightType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "leftType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.errors.UnificationInferenceError
unificationInferenceError :: Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Errors.UnificationError -> Typed.TypedTerm Errors.UnificationInferenceError
unificationInferenceError path cause =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm path)},
        Core.Field {
          Core.fieldName = (Core.Name "cause"),
          Core.fieldTerm = (Typed.unTypedTerm cause)}]}))
-- | DSL accessor for the cause field of hydra.errors.UnificationInferenceError
unificationInferenceErrorCause :: Typed.TypedTerm Errors.UnificationInferenceError -> Typed.TypedTerm Errors.UnificationError
unificationInferenceErrorCause x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
        Core.projectionFieldName = (Core.Name "cause")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the path field of hydra.errors.UnificationInferenceError
unificationInferenceErrorPath :: Typed.TypedTerm Errors.UnificationInferenceError -> Typed.TypedTerm Paths.SubtermPath
unificationInferenceErrorPath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cause field of hydra.errors.UnificationInferenceError
unificationInferenceErrorWithCause :: Typed.TypedTerm Errors.UnificationInferenceError -> Typed.TypedTerm Errors.UnificationError -> Typed.TypedTerm Errors.UnificationInferenceError
unificationInferenceErrorWithCause original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cause"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the path field of hydra.errors.UnificationInferenceError
unificationInferenceErrorWithPath :: Typed.TypedTerm Errors.UnificationInferenceError -> Typed.TypedTerm Paths.SubtermPath -> Typed.TypedTerm Errors.UnificationInferenceError
unificationInferenceErrorWithPath original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
              Core.projectionFieldName = (Core.Name "cause")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
