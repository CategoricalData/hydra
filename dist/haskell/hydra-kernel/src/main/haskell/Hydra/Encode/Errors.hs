-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.errors

module Hydra.Encode.Errors where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Error.Checking as Checking
import qualified Hydra.Encode.Error.Core as ErrorCore
import qualified Hydra.Encode.Paths as Paths
import qualified Hydra.Errors as Errors
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
decodingError :: Errors.DecodingError -> Core.Term
decodingError x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Errors.unDecodingError x))})
emptyListError :: t0 -> Core.Term
emptyListError _ = Core.TermUnit
error :: Errors.Error -> Core.Term
error x =
    case x of
      Errors.ErrorChecking v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "checking"),
          Core.fieldTerm = (Checking.checkingError v0)}})
      Errors.ErrorDecoding v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "decoding"),
          Core.fieldTerm = (decodingError v0)}})
      Errors.ErrorDuplicateBinding v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateBinding"),
          Core.fieldTerm = (ErrorCore.duplicateBindingError v0)}})
      Errors.ErrorDuplicateField v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duplicateField"),
          Core.fieldTerm = (ErrorCore.duplicateFieldError v0)}})
      Errors.ErrorExtraction v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "extraction"),
          Core.fieldTerm = (extractionError v0)}})
      Errors.ErrorInference v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "inference"),
          Core.fieldTerm = (inferenceError v0)}})
      Errors.ErrorOther v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (otherError v0)}})
      Errors.ErrorResolution v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "resolution"),
          Core.fieldTerm = (resolutionError v0)}})
      Errors.ErrorUndefinedField v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedField"),
          Core.fieldTerm = (ErrorCore.undefinedFieldError v0)}})
      Errors.ErrorUndefinedTermVariable v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undefinedTermVariable"),
          Core.fieldTerm = (ErrorCore.undefinedTermVariableError v0)}})
      Errors.ErrorUntypedTermVariable v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "untypedTermVariable"),
          Core.fieldTerm = (ErrorCore.untypedTermVariableError v0)}})
      Errors.ErrorUnexpectedTermVariant v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unexpectedTermVariant"),
          Core.fieldTerm = (ErrorCore.unexpectedTermVariantError v0)}})
      Errors.ErrorUnexpectedTypeVariant v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unexpectedTypeVariant"),
          Core.fieldTerm = (ErrorCore.unexpectedTypeVariantError v0)}})
      Errors.ErrorUnification v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unification"),
          Core.fieldTerm = (unificationError v0)}})
extractionError :: Errors.ExtractionError -> Core.Term
extractionError x =
    case x of
      Errors.ExtractionErrorEmptyList v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "emptyList"),
          Core.fieldTerm = (emptyListError v0)}})
      Errors.ExtractionErrorMultipleBindings v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "multipleBindings"),
          Core.fieldTerm = (multipleBindingsError v0)}})
      Errors.ExtractionErrorMultipleFields v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "multipleFields"),
          Core.fieldTerm = (multipleFieldsError v0)}})
      Errors.ExtractionErrorNoMatchingField v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "noMatchingField"),
          Core.fieldTerm = (noMatchingFieldError v0)}})
      Errors.ExtractionErrorNoSuchBinding v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "noSuchBinding"),
          Core.fieldTerm = (noSuchBindingError v0)}})
      Errors.ExtractionErrorNotEnoughCases v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "notEnoughCases"),
          Core.fieldTerm = (notEnoughCasesError v0)}})
      Errors.ExtractionErrorUnexpectedShape v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unexpectedShape"),
          Core.fieldTerm = (unexpectedShapeError v0)}})
inferenceError :: Errors.InferenceError -> Core.Term
inferenceError x =
    case x of
      Errors.InferenceErrorChecking v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "checking"),
          Core.fieldTerm = (Checking.checkingError v0)}})
      Errors.InferenceErrorOther v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (otherInferenceError v0)}})
      Errors.InferenceErrorUnification v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unification"),
          Core.fieldTerm = (unificationInferenceError v0)}})
multipleBindingsError :: Errors.MultipleBindingsError -> Core.Term
multipleBindingsError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (Errors.multipleBindingsErrorName x))}]})
multipleFieldsError :: Errors.MultipleFieldsError -> Core.Term
multipleFieldsError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (EncodeCore.name (Errors.multipleFieldsErrorFieldName x))}]})
noMatchingFieldError :: Errors.NoMatchingFieldError -> Core.Term
noMatchingFieldError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fieldName"),
          Core.fieldTerm = (EncodeCore.name (Errors.noMatchingFieldErrorFieldName x))}]})
noSuchBindingError :: Errors.NoSuchBindingError -> Core.Term
noSuchBindingError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (Errors.noSuchBindingErrorName x))}]})
noSuchPrimitiveError :: Errors.NoSuchPrimitiveError -> Core.Term
noSuchPrimitiveError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (Errors.noSuchPrimitiveErrorName x))}]})
notEnoughCasesError :: t0 -> Core.Term
notEnoughCasesError _ = Core.TermUnit
otherError :: Errors.OtherError -> Core.Term
otherError x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherError"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Errors.unOtherError x))})
otherInferenceError :: Errors.OtherInferenceError -> Core.Term
otherInferenceError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Paths.subtermPath (Errors.otherInferenceErrorPath x))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Errors.otherInferenceErrorMessage x))}]})
otherResolutionError :: Errors.OtherResolutionError -> Core.Term
otherResolutionError x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherResolutionError"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Errors.unOtherResolutionError x))})
resolutionError :: Errors.ResolutionError -> Core.Term
resolutionError x =
    case x of
      Errors.ResolutionErrorNoSuchBinding v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "noSuchBinding"),
          Core.fieldTerm = (noSuchBindingError v0)}})
      Errors.ResolutionErrorNoSuchPrimitive v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "noSuchPrimitive"),
          Core.fieldTerm = (noSuchPrimitiveError v0)}})
      Errors.ResolutionErrorNoMatchingField v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "noMatchingField"),
          Core.fieldTerm = (noMatchingFieldError v0)}})
      Errors.ResolutionErrorOther v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (otherResolutionError v0)}})
      Errors.ResolutionErrorUnexpectedShape v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unexpectedShape"),
          Core.fieldTerm = (unexpectedShapeError v0)}})
unexpectedShapeError :: Errors.UnexpectedShapeError -> Core.Term
unexpectedShapeError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Errors.unexpectedShapeErrorExpected x))},
        Core.Field {
          Core.fieldName = (Core.Name "actual"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Errors.unexpectedShapeErrorActual x))}]})
unificationError :: Errors.UnificationError -> Core.Term
unificationError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftType"),
          Core.fieldTerm = (EncodeCore.type_ (Errors.unificationErrorLeftType x))},
        Core.Field {
          Core.fieldName = (Core.Name "rightType"),
          Core.fieldTerm = (EncodeCore.type_ (Errors.unificationErrorRightType x))},
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Errors.unificationErrorMessage x))}]})
unificationInferenceError :: Errors.UnificationInferenceError -> Core.Term
unificationInferenceError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Paths.subtermPath (Errors.unificationInferenceErrorPath x))},
        Core.Field {
          Core.fieldName = (Core.Name "cause"),
          Core.fieldTerm = (unificationError (Errors.unificationInferenceErrorCause x))}]})
