-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.errors

module Hydra.Dsl.Errors where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

decodingError :: Phantoms.TTerm String -> Phantoms.TTerm Errors.DecodingError
decodingError x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

errorChecking :: Phantoms.TTerm Checking.CheckingError -> Phantoms.TTerm Errors.Error
errorChecking x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "checking"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorDecoding :: Phantoms.TTerm Errors.DecodingError -> Phantoms.TTerm Errors.Error
errorDecoding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decoding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorDuplicateBinding :: Phantoms.TTerm ErrorCore.DuplicateBindingError -> Phantoms.TTerm Errors.Error
errorDuplicateBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorDuplicateField :: Phantoms.TTerm ErrorCore.DuplicateFieldError -> Phantoms.TTerm Errors.Error
errorDuplicateField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duplicateField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorExtraction :: Phantoms.TTerm Errors.ExtractionError -> Phantoms.TTerm Errors.Error
errorExtraction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extraction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorInference :: Phantoms.TTerm Errors.InferenceError -> Phantoms.TTerm Errors.Error
errorInference x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorOther :: Phantoms.TTerm Errors.OtherError -> Phantoms.TTerm Errors.Error
errorOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorResolution :: Phantoms.TTerm Errors.ResolutionError -> Phantoms.TTerm Errors.Error
errorResolution x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "resolution"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUndefinedField :: Phantoms.TTerm ErrorCore.UndefinedFieldError -> Phantoms.TTerm Errors.Error
errorUndefinedField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUndefinedTermVariable :: Phantoms.TTerm ErrorCore.UndefinedTermVariableError -> Phantoms.TTerm Errors.Error
errorUndefinedTermVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefinedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUnexpectedTermVariant :: Phantoms.TTerm ErrorCore.UnexpectedTermVariantError -> Phantoms.TTerm Errors.Error
errorUnexpectedTermVariant x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedTermVariant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUnexpectedTypeVariant :: Phantoms.TTerm ErrorCore.UnexpectedTypeVariantError -> Phantoms.TTerm Errors.Error
errorUnexpectedTypeVariant x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedTypeVariant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUnification :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Errors.Error
errorUnification x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unification"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

errorUntypedTermVariable :: Phantoms.TTerm ErrorCore.UntypedTermVariableError -> Phantoms.TTerm Errors.Error
errorUntypedTermVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untypedTermVariable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

extractionErrorEmptyList :: Phantoms.TTerm Errors.EmptyListError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorEmptyList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyList"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

extractionErrorMultipleBindings :: Phantoms.TTerm Errors.MultipleBindingsError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorMultipleBindings x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multipleBindings"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

extractionErrorMultipleFields :: Phantoms.TTerm Errors.MultipleFieldsError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorMultipleFields x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multipleFields"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

extractionErrorNoMatchingField :: Phantoms.TTerm Errors.NoMatchingFieldError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorNoMatchingField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noMatchingField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

extractionErrorNoSuchBinding :: Phantoms.TTerm Errors.NoSuchBindingError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorNoSuchBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noSuchBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

extractionErrorNotEnoughCases :: Phantoms.TTerm Errors.NotEnoughCasesError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorNotEnoughCases x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEnoughCases"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

extractionErrorUnexpectedShape :: Phantoms.TTerm Errors.UnexpectedShapeError -> Phantoms.TTerm Errors.ExtractionError
extractionErrorUnexpectedShape x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedShape"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inferenceErrorChecking :: Phantoms.TTerm Checking.CheckingError -> Phantoms.TTerm Errors.InferenceError
inferenceErrorChecking x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "checking"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inferenceErrorOther :: Phantoms.TTerm Errors.OtherInferenceError -> Phantoms.TTerm Errors.InferenceError
inferenceErrorOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

inferenceErrorUnification :: Phantoms.TTerm Errors.UnificationInferenceError -> Phantoms.TTerm Errors.InferenceError
inferenceErrorUnification x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unification"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multipleBindingsError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.MultipleBindingsError
multipleBindingsError name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

multipleBindingsErrorName :: Phantoms.TTerm Errors.MultipleBindingsError -> Phantoms.TTerm Core.Name
multipleBindingsErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multipleBindingsErrorWithName :: Phantoms.TTerm Errors.MultipleBindingsError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.MultipleBindingsError
multipleBindingsErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multipleFieldsError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.MultipleFieldsError
multipleFieldsError fieldName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm fieldName)}]}))

multipleFieldsErrorFieldName :: Phantoms.TTerm Errors.MultipleFieldsError -> Phantoms.TTerm Core.Name
multipleFieldsErrorFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
        Core.projectionField = (Core.Name "fieldName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multipleFieldsErrorWithFieldName :: Phantoms.TTerm Errors.MultipleFieldsError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.MultipleFieldsError
multipleFieldsErrorWithFieldName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

noMatchingFieldError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoMatchingFieldError
noMatchingFieldError fieldName =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm fieldName)}]}))

noMatchingFieldErrorFieldName :: Phantoms.TTerm Errors.NoMatchingFieldError -> Phantoms.TTerm Core.Name
noMatchingFieldErrorFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
        Core.projectionField = (Core.Name "fieldName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

noMatchingFieldErrorWithFieldName :: Phantoms.TTerm Errors.NoMatchingFieldError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoMatchingFieldError
noMatchingFieldErrorWithFieldName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

noSuchBindingError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoSuchBindingError
noSuchBindingError name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

noSuchBindingErrorName :: Phantoms.TTerm Errors.NoSuchBindingError -> Phantoms.TTerm Core.Name
noSuchBindingErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

noSuchBindingErrorWithName :: Phantoms.TTerm Errors.NoSuchBindingError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoSuchBindingError
noSuchBindingErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

noSuchPrimitiveError :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoSuchPrimitiveError
noSuchPrimitiveError name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))

noSuchPrimitiveErrorName :: Phantoms.TTerm Errors.NoSuchPrimitiveError -> Phantoms.TTerm Core.Name
noSuchPrimitiveErrorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

noSuchPrimitiveErrorWithName :: Phantoms.TTerm Errors.NoSuchPrimitiveError -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Errors.NoSuchPrimitiveError
noSuchPrimitiveErrorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

otherError :: Phantoms.TTerm String -> Phantoms.TTerm Errors.OtherError
otherError x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherError"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

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

otherInferenceErrorMessage :: Phantoms.TTerm Errors.OtherInferenceError -> Phantoms.TTerm String
otherInferenceErrorMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
        Core.projectionField = (Core.Name "message")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

otherInferenceErrorPath :: Phantoms.TTerm Errors.OtherInferenceError -> Phantoms.TTerm Paths.SubtermPath
otherInferenceErrorPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

otherResolutionError :: Phantoms.TTerm String -> Phantoms.TTerm Errors.OtherResolutionError
otherResolutionError x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherResolutionError"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

resolutionErrorNoMatchingField :: Phantoms.TTerm Errors.NoMatchingFieldError -> Phantoms.TTerm Errors.ResolutionError
resolutionErrorNoMatchingField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noMatchingField"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

resolutionErrorNoSuchBinding :: Phantoms.TTerm Errors.NoSuchBindingError -> Phantoms.TTerm Errors.ResolutionError
resolutionErrorNoSuchBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noSuchBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

resolutionErrorNoSuchPrimitive :: Phantoms.TTerm Errors.NoSuchPrimitiveError -> Phantoms.TTerm Errors.ResolutionError
resolutionErrorNoSuchPrimitive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noSuchPrimitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

resolutionErrorOther :: Phantoms.TTerm Errors.OtherResolutionError -> Phantoms.TTerm Errors.ResolutionError
resolutionErrorOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

resolutionErrorUnexpectedShape :: Phantoms.TTerm Errors.UnexpectedShapeError -> Phantoms.TTerm Errors.ResolutionError
resolutionErrorUnexpectedShape x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unexpectedShape"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unDecodingError :: Phantoms.TTerm Errors.DecodingError -> Phantoms.TTerm String
unDecodingError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.errors.DecodingError")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unOtherError :: Phantoms.TTerm Errors.OtherError -> Phantoms.TTerm String
unOtherError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.errors.OtherError")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unOtherResolutionError :: Phantoms.TTerm Errors.OtherResolutionError -> Phantoms.TTerm String
unOtherResolutionError x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.errors.OtherResolutionError")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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

unexpectedShapeErrorActual :: Phantoms.TTerm Errors.UnexpectedShapeError -> Phantoms.TTerm String
unexpectedShapeErrorActual x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
        Core.projectionField = (Core.Name "actual")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unexpectedShapeErrorExpected :: Phantoms.TTerm Errors.UnexpectedShapeError -> Phantoms.TTerm String
unexpectedShapeErrorExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
        Core.projectionField = (Core.Name "expected")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
              Core.projectionField = (Core.Name "expected")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
              Core.projectionField = (Core.Name "actual")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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

unificationErrorLeftType :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Core.Type
unificationErrorLeftType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionField = (Core.Name "leftType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unificationErrorMessage :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm String
unificationErrorMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionField = (Core.Name "message")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unificationErrorRightType :: Phantoms.TTerm Errors.UnificationError -> Phantoms.TTerm Core.Type
unificationErrorRightType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
        Core.projectionField = (Core.Name "rightType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
              Core.projectionField = (Core.Name "rightType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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
              Core.projectionField = (Core.Name "leftType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionField = (Core.Name "rightType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
              Core.projectionField = (Core.Name "leftType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.errors.UnificationError"),
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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

unificationInferenceErrorCause :: Phantoms.TTerm Errors.UnificationInferenceError -> Phantoms.TTerm Errors.UnificationError
unificationInferenceErrorCause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
        Core.projectionField = (Core.Name "cause")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unificationInferenceErrorPath :: Phantoms.TTerm Errors.UnificationInferenceError -> Phantoms.TTerm Paths.SubtermPath
unificationInferenceErrorPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
        Core.projectionField = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

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
              Core.projectionField = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cause"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
              Core.projectionField = (Core.Name "cause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
