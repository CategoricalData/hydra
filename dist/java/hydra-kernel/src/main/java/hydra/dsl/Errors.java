// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.errors
 */
public interface Errors {
  static hydra.phantoms.TTerm<hydra.errors.DecodingError> decodingError(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorChecking(hydra.phantoms.TTerm<hydra.error.checking.CheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("checking"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorDecoding(hydra.phantoms.TTerm<hydra.errors.DecodingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("decoding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorDuplicateBinding(hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("duplicateBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorDuplicateField(hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("duplicateField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorExtraction(hydra.phantoms.TTerm<hydra.errors.ExtractionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("extraction"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorInference(hydra.phantoms.TTerm<hydra.errors.InferenceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("inference"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorOther(hydra.phantoms.TTerm<hydra.errors.OtherError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("other"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorResolution(hydra.phantoms.TTerm<hydra.errors.ResolutionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("resolution"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUndefinedField(hydra.phantoms.TTerm<hydra.error.core.UndefinedFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("undefinedField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUndefinedTermVariable(hydra.phantoms.TTerm<hydra.error.core.UndefinedTermVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("undefinedTermVariable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUnexpectedTermVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("unexpectedTermVariant"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUnexpectedTypeVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("unexpectedTypeVariant"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUnification(hydra.phantoms.TTerm<hydra.errors.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("unification"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUntypedTermVariable(hydra.phantoms.TTerm<hydra.error.core.UntypedTermVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("untypedTermVariable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ExtractionError> extractionErrorEmptyList(hydra.phantoms.TTerm<java.lang.Void> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("emptyList"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ExtractionError> extractionErrorMultipleBindings(hydra.phantoms.TTerm<hydra.errors.MultipleBindingsError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("multipleBindings"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ExtractionError> extractionErrorMultipleFields(hydra.phantoms.TTerm<hydra.errors.MultipleFieldsError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("multipleFields"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ExtractionError> extractionErrorNoMatchingField(hydra.phantoms.TTerm<hydra.errors.NoMatchingFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("noMatchingField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ExtractionError> extractionErrorNoSuchBinding(hydra.phantoms.TTerm<hydra.errors.NoSuchBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("noSuchBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ExtractionError> extractionErrorNotEnoughCases(hydra.phantoms.TTerm<java.lang.Void> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("notEnoughCases"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ExtractionError> extractionErrorUnexpectedShape(hydra.phantoms.TTerm<hydra.errors.UnexpectedShapeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("unexpectedShape"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.InferenceError> inferenceErrorChecking(hydra.phantoms.TTerm<hydra.error.checking.CheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.InferenceError"), new hydra.core.Field(new hydra.core.Name("checking"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.InferenceError> inferenceErrorOther(hydra.phantoms.TTerm<hydra.errors.OtherInferenceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.InferenceError"), new hydra.core.Field(new hydra.core.Name("other"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.InferenceError> inferenceErrorUnification(hydra.phantoms.TTerm<hydra.errors.UnificationInferenceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.InferenceError"), new hydra.core.Field(new hydra.core.Name("unification"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.MultipleBindingsError> multipleBindingsError(hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.MultipleBindingsError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> multipleBindingsErrorName(hydra.phantoms.TTerm<hydra.errors.MultipleBindingsError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.MultipleBindingsError"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.MultipleBindingsError> multipleBindingsErrorWithName(hydra.phantoms.TTerm<hydra.errors.MultipleBindingsError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.MultipleBindingsError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.MultipleFieldsError> multipleFieldsError(hydra.phantoms.TTerm<hydra.core.Name> fieldName) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.MultipleFieldsError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("fieldName"), (fieldName).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> multipleFieldsErrorFieldName(hydra.phantoms.TTerm<hydra.errors.MultipleFieldsError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.MultipleFieldsError"), new hydra.core.Name("fieldName"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.MultipleFieldsError> multipleFieldsErrorWithFieldName(hydra.phantoms.TTerm<hydra.errors.MultipleFieldsError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.MultipleFieldsError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("fieldName"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.NoMatchingFieldError> noMatchingFieldError(hydra.phantoms.TTerm<hydra.core.Name> fieldName) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.NoMatchingFieldError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("fieldName"), (fieldName).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> noMatchingFieldErrorFieldName(hydra.phantoms.TTerm<hydra.errors.NoMatchingFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.NoMatchingFieldError"), new hydra.core.Name("fieldName"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.NoMatchingFieldError> noMatchingFieldErrorWithFieldName(hydra.phantoms.TTerm<hydra.errors.NoMatchingFieldError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.NoMatchingFieldError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("fieldName"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.NoSuchBindingError> noSuchBindingError(hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.NoSuchBindingError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> noSuchBindingErrorName(hydra.phantoms.TTerm<hydra.errors.NoSuchBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.NoSuchBindingError"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.NoSuchBindingError> noSuchBindingErrorWithName(hydra.phantoms.TTerm<hydra.errors.NoSuchBindingError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.NoSuchBindingError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.NoSuchPrimitiveError> noSuchPrimitiveError(hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.NoSuchPrimitiveError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> noSuchPrimitiveErrorName(hydra.phantoms.TTerm<hydra.errors.NoSuchPrimitiveError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.NoSuchPrimitiveError"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.NoSuchPrimitiveError> noSuchPrimitiveErrorWithName(hydra.phantoms.TTerm<hydra.errors.NoSuchPrimitiveError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.NoSuchPrimitiveError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.OtherError> otherError(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.OtherError"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.OtherInferenceError> otherInferenceError(hydra.phantoms.TTerm<hydra.paths.SubtermPath> path, hydra.phantoms.TTerm<String> message) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.OtherInferenceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (path).value),
      new hydra.core.Field(new hydra.core.Name("message"), (message).value)))));
  }

  static hydra.phantoms.TTerm<String> otherInferenceErrorMessage(hydra.phantoms.TTerm<hydra.errors.OtherInferenceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.OtherInferenceError"), new hydra.core.Name("message"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermPath> otherInferenceErrorPath(hydra.phantoms.TTerm<hydra.errors.OtherInferenceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.OtherInferenceError"), new hydra.core.Name("path"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.OtherInferenceError> otherInferenceErrorWithMessage(hydra.phantoms.TTerm<hydra.errors.OtherInferenceError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.OtherInferenceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.OtherInferenceError"), new hydra.core.Name("path"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("message"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.OtherInferenceError> otherInferenceErrorWithPath(hydra.phantoms.TTerm<hydra.errors.OtherInferenceError> original, hydra.phantoms.TTerm<hydra.paths.SubtermPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.OtherInferenceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.OtherInferenceError"), new hydra.core.Name("message"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.errors.OtherResolutionError> otherResolutionError(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.OtherResolutionError"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.ResolutionError> resolutionErrorNoMatchingField(hydra.phantoms.TTerm<hydra.errors.NoMatchingFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ResolutionError"), new hydra.core.Field(new hydra.core.Name("noMatchingField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ResolutionError> resolutionErrorNoSuchBinding(hydra.phantoms.TTerm<hydra.errors.NoSuchBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ResolutionError"), new hydra.core.Field(new hydra.core.Name("noSuchBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ResolutionError> resolutionErrorNoSuchPrimitive(hydra.phantoms.TTerm<hydra.errors.NoSuchPrimitiveError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ResolutionError"), new hydra.core.Field(new hydra.core.Name("noSuchPrimitive"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ResolutionError> resolutionErrorOther(hydra.phantoms.TTerm<hydra.errors.OtherResolutionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ResolutionError"), new hydra.core.Field(new hydra.core.Name("other"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.ResolutionError> resolutionErrorUnexpectedShape(hydra.phantoms.TTerm<hydra.errors.UnexpectedShapeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ResolutionError"), new hydra.core.Field(new hydra.core.Name("unexpectedShape"), (x).value))));
  }

  static hydra.phantoms.TTerm<String> unDecodingError(hydra.phantoms.TTerm<hydra.errors.DecodingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.errors.DecodingError")), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unOtherError(hydra.phantoms.TTerm<hydra.errors.OtherError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.errors.OtherError")), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unOtherResolutionError(hydra.phantoms.TTerm<hydra.errors.OtherResolutionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.errors.OtherResolutionError")), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnexpectedShapeError> unexpectedShapeError(hydra.phantoms.TTerm<String> expected, hydra.phantoms.TTerm<String> actual) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnexpectedShapeError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value),
      new hydra.core.Field(new hydra.core.Name("actual"), (actual).value)))));
  }

  static hydra.phantoms.TTerm<String> unexpectedShapeErrorActual(hydra.phantoms.TTerm<hydra.errors.UnexpectedShapeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnexpectedShapeError"), new hydra.core.Name("actual"))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unexpectedShapeErrorExpected(hydra.phantoms.TTerm<hydra.errors.UnexpectedShapeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnexpectedShapeError"), new hydra.core.Name("expected"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnexpectedShapeError> unexpectedShapeErrorWithActual(hydra.phantoms.TTerm<hydra.errors.UnexpectedShapeError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnexpectedShapeError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnexpectedShapeError"), new hydra.core.Name("expected"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actual"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnexpectedShapeError> unexpectedShapeErrorWithExpected(hydra.phantoms.TTerm<hydra.errors.UnexpectedShapeError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnexpectedShapeError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actual"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnexpectedShapeError"), new hydra.core.Name("actual"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationError> unificationError(hydra.phantoms.TTerm<hydra.core.Type> leftType, hydra.phantoms.TTerm<hydra.core.Type> rightType, hydra.phantoms.TTerm<String> message) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("leftType"), (leftType).value),
      new hydra.core.Field(new hydra.core.Name("rightType"), (rightType).value),
      new hydra.core.Field(new hydra.core.Name("message"), (message).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unificationErrorLeftType(hydra.phantoms.TTerm<hydra.errors.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("leftType"))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unificationErrorMessage(hydra.phantoms.TTerm<hydra.errors.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("message"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unificationErrorRightType(hydra.phantoms.TTerm<hydra.errors.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("rightType"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationError> unificationErrorWithLeftType(hydra.phantoms.TTerm<hydra.errors.UnificationError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("leftType"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rightType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("rightType"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("message"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationError> unificationErrorWithMessage(hydra.phantoms.TTerm<hydra.errors.UnificationError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("leftType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("leftType"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rightType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("rightType"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("message"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationError> unificationErrorWithRightType(hydra.phantoms.TTerm<hydra.errors.UnificationError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("leftType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("leftType"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rightType"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("message"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationInferenceError> unificationInferenceError(hydra.phantoms.TTerm<hydra.paths.SubtermPath> path, hydra.phantoms.TTerm<hydra.errors.UnificationError> cause) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationInferenceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (path).value),
      new hydra.core.Field(new hydra.core.Name("cause"), (cause).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationError> unificationInferenceErrorCause(hydra.phantoms.TTerm<hydra.errors.UnificationInferenceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationInferenceError"), new hydra.core.Name("cause"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermPath> unificationInferenceErrorPath(hydra.phantoms.TTerm<hydra.errors.UnificationInferenceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationInferenceError"), new hydra.core.Name("path"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationInferenceError> unificationInferenceErrorWithCause(hydra.phantoms.TTerm<hydra.errors.UnificationInferenceError> original, hydra.phantoms.TTerm<hydra.errors.UnificationError> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationInferenceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationInferenceError"), new hydra.core.Name("path"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("cause"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationInferenceError> unificationInferenceErrorWithPath(hydra.phantoms.TTerm<hydra.errors.UnificationInferenceError> original, hydra.phantoms.TTerm<hydra.paths.SubtermPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationInferenceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("cause"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationInferenceError"), new hydra.core.Name("cause"))), (original).value)))))));
  }
}
