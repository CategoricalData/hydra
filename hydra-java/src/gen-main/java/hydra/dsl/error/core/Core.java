// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.error.core;

/**
 * DSL functions for hydra.error.core
 */
public interface Core {
  static hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> duplicateBindingError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> duplicateBindingErrorLocation(hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateBindingError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> duplicateBindingErrorName(hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateBindingError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> duplicateBindingErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateBindingError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> duplicateBindingErrorWithName(hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateBindingError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> duplicateFieldError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> duplicateFieldErrorLocation(hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateFieldError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> duplicateFieldErrorName(hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateFieldError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> duplicateFieldErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateFieldError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> duplicateFieldErrorWithName(hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateFieldError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorDuplicateBinding(hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("duplicateBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorDuplicateField(hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("duplicateField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedFieldError> undefinedFieldError(hydra.phantoms.TTerm<hydra.core.Name> fieldName, hydra.phantoms.TTerm<hydra.core.Name> typeName) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fieldName"), (fieldName).value),
      new hydra.core.Field(new hydra.core.Name("typeName"), (typeName).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedFieldErrorFieldName(hydra.phantoms.TTerm<hydra.error.core.UndefinedFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedFieldError"), new hydra.core.Name("fieldName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedFieldErrorTypeName(hydra.phantoms.TTerm<hydra.error.core.UndefinedFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedFieldError"), new hydra.core.Name("typeName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedFieldError> undefinedFieldErrorWithFieldName(hydra.phantoms.TTerm<hydra.error.core.UndefinedFieldError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fieldName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedFieldError"), new hydra.core.Name("typeName"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedFieldError> undefinedFieldErrorWithTypeName(hydra.phantoms.TTerm<hydra.error.core.UndefinedFieldError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fieldName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedFieldError"), new hydra.core.Name("fieldName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeName"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTermError> undefinedTermError(hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTermError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedTermErrorName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTermError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTermError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTermError> undefinedTermErrorWithName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTermError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTermError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeError> undefinedTypeError(hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedTypeErrorName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeError> undefinedTypeErrorWithName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> unexpectedTermVariantError(hydra.phantoms.TTerm<hydra.variants.TermVariant> expectedVariant, hydra.phantoms.TTerm<hydra.core.Term> actualTerm) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (expectedVariant).value),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), (actualTerm).value)))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> unexpectedTermVariantErrorExpectedVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), new hydra.core.Name("expectedVariant"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> unexpectedTermVariantErrorActualTerm(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), new hydra.core.Name("actualTerm"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> unexpectedTermVariantErrorWithExpectedVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> original, hydra.phantoms.TTerm<hydra.variants.TermVariant> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), new hydra.core.Name("actualTerm"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> unexpectedTermVariantErrorWithActualTerm(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), new hydra.core.Name("expectedVariant"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> unexpectedTypeVariantError(hydra.phantoms.TTerm<hydra.variants.TypeVariant> expectedVariant, hydra.phantoms.TTerm<hydra.core.Type> actualType) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (expectedVariant).value),
      new hydra.core.Field(new hydra.core.Name("actualType"), (actualType).value)))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> unexpectedTypeVariantErrorExpectedVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), new hydra.core.Name("expectedVariant"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unexpectedTypeVariantErrorActualType(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), new hydra.core.Name("actualType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> unexpectedTypeVariantErrorWithExpectedVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> original, hydra.phantoms.TTerm<hydra.variants.TypeVariant> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actualType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), new hydra.core.Name("actualType"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> unexpectedTypeVariantErrorWithActualType(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), new hydra.core.Name("expectedVariant"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualType"), (newVal).value)))));
  }
}
