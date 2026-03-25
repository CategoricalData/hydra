// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.error;

/**
 * DSL functions for hydra.error.core
 */
public interface Core {
  static hydra.phantoms.TTerm<hydra.error.core.ConstantConditionError> constantConditionError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<Boolean> value) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.ConstantConditionError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("value"), (value).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> constantConditionErrorLocation(hydra.phantoms.TTerm<hydra.error.core.ConstantConditionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.ConstantConditionError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<Boolean> constantConditionErrorValue(hydra.phantoms.TTerm<hydra.error.core.ConstantConditionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.ConstantConditionError"), new hydra.core.Name("value"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.ConstantConditionError> constantConditionErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.ConstantConditionError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.ConstantConditionError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.ConstantConditionError"), new hydra.core.Name("value"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.ConstantConditionError> constantConditionErrorWithValue(hydra.phantoms.TTerm<hydra.error.core.ConstantConditionError> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.ConstantConditionError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.ConstantConditionError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("value"), (newVal).value)))));
  }

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

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateRecordTypeFieldNamesError> duplicateRecordTypeFieldNamesError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> duplicateRecordTypeFieldNamesErrorLocation(hydra.phantoms.TTerm<hydra.error.core.DuplicateRecordTypeFieldNamesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> duplicateRecordTypeFieldNamesErrorName(hydra.phantoms.TTerm<hydra.error.core.DuplicateRecordTypeFieldNamesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateRecordTypeFieldNamesError> duplicateRecordTypeFieldNamesErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.DuplicateRecordTypeFieldNamesError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateRecordTypeFieldNamesError> duplicateRecordTypeFieldNamesErrorWithName(hydra.phantoms.TTerm<hydra.error.core.DuplicateRecordTypeFieldNamesError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateUnionTypeFieldNamesError> duplicateUnionTypeFieldNamesError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> duplicateUnionTypeFieldNamesErrorLocation(hydra.phantoms.TTerm<hydra.error.core.DuplicateUnionTypeFieldNamesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> duplicateUnionTypeFieldNamesErrorName(hydra.phantoms.TTerm<hydra.error.core.DuplicateUnionTypeFieldNamesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateUnionTypeFieldNamesError> duplicateUnionTypeFieldNamesErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.DuplicateUnionTypeFieldNamesError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.DuplicateUnionTypeFieldNamesError> duplicateUnionTypeFieldNamesErrorWithName(hydra.phantoms.TTerm<hydra.error.core.DuplicateUnionTypeFieldNamesError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyCaseStatementError> emptyCaseStatementError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> typeName) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("typeName"), (typeName).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> emptyCaseStatementErrorLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyCaseStatementError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> emptyCaseStatementErrorTypeName(hydra.phantoms.TTerm<hydra.error.core.EmptyCaseStatementError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), new hydra.core.Name("typeName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyCaseStatementError> emptyCaseStatementErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyCaseStatementError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), new hydra.core.Name("typeName"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyCaseStatementError> emptyCaseStatementErrorWithTypeName(hydra.phantoms.TTerm<hydra.error.core.EmptyCaseStatementError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeName"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyLetBindingsError> emptyLetBindingsError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyLetBindingsError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (location).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> emptyLetBindingsErrorLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyLetBindingsError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.EmptyLetBindingsError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyLetBindingsError> emptyLetBindingsErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyLetBindingsError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyLetBindingsError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyRecordTypeError> emptyRecordTypeError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyRecordTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (location).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> emptyRecordTypeErrorLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyRecordTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.EmptyRecordTypeError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyRecordTypeError> emptyRecordTypeErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyRecordTypeError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyRecordTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyTermAnnotationError> emptyTermAnnotationError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyTermAnnotationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (location).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> emptyTermAnnotationErrorLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyTermAnnotationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.EmptyTermAnnotationError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyTermAnnotationError> emptyTermAnnotationErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyTermAnnotationError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyTermAnnotationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyTypeAnnotationError> emptyTypeAnnotationError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyTypeAnnotationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (location).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> emptyTypeAnnotationErrorLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyTypeAnnotationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.EmptyTypeAnnotationError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyTypeAnnotationError> emptyTypeAnnotationErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyTypeAnnotationError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyTypeAnnotationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyTypeNameInTermError> emptyTypeNameInTermError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyTypeNameInTermError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (location).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> emptyTypeNameInTermErrorLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyTypeNameInTermError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.EmptyTypeNameInTermError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyTypeNameInTermError> emptyTypeNameInTermErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyTypeNameInTermError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyTypeNameInTermError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyUnionTypeError> emptyUnionTypeError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyUnionTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (location).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> emptyUnionTypeErrorLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyUnionTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.EmptyUnionTypeError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.EmptyUnionTypeError> emptyUnionTypeErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.EmptyUnionTypeError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyUnionTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidForallParameterNameError> invalidForallParameterNameError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> invalidForallParameterNameErrorLocation(hydra.phantoms.TTerm<hydra.error.core.InvalidForallParameterNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> invalidForallParameterNameErrorName(hydra.phantoms.TTerm<hydra.error.core.InvalidForallParameterNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidForallParameterNameError> invalidForallParameterNameErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.InvalidForallParameterNameError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidForallParameterNameError> invalidForallParameterNameErrorWithName(hydra.phantoms.TTerm<hydra.error.core.InvalidForallParameterNameError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidLambdaParameterNameError> invalidLambdaParameterNameError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> invalidLambdaParameterNameErrorLocation(hydra.phantoms.TTerm<hydra.error.core.InvalidLambdaParameterNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> invalidLambdaParameterNameErrorName(hydra.phantoms.TTerm<hydra.error.core.InvalidLambdaParameterNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidLambdaParameterNameError> invalidLambdaParameterNameErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.InvalidLambdaParameterNameError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidLambdaParameterNameError> invalidLambdaParameterNameErrorWithName(hydra.phantoms.TTerm<hydra.error.core.InvalidLambdaParameterNameError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidLetBindingNameError> invalidLetBindingNameError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> invalidLetBindingNameErrorLocation(hydra.phantoms.TTerm<hydra.error.core.InvalidLetBindingNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> invalidLetBindingNameErrorName(hydra.phantoms.TTerm<hydra.error.core.InvalidLetBindingNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidLetBindingNameError> invalidLetBindingNameErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.InvalidLetBindingNameError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidLetBindingNameError> invalidLetBindingNameErrorWithName(hydra.phantoms.TTerm<hydra.error.core.InvalidLetBindingNameError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorConstantCondition(hydra.phantoms.TTerm<hydra.error.core.ConstantConditionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("constantCondition"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorDuplicateBinding(hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("duplicateBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorDuplicateField(hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("duplicateField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorEmptyCaseStatement(hydra.phantoms.TTerm<hydra.error.core.EmptyCaseStatementError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("emptyCaseStatement"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorEmptyLetBindings(hydra.phantoms.TTerm<hydra.error.core.EmptyLetBindingsError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("emptyLetBindings"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorEmptyTermAnnotation(hydra.phantoms.TTerm<hydra.error.core.EmptyTermAnnotationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("emptyTermAnnotation"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorEmptyTypeNameInTerm(hydra.phantoms.TTerm<hydra.error.core.EmptyTypeNameInTermError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("emptyTypeNameInTerm"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorInvalidLambdaParameterName(hydra.phantoms.TTerm<hydra.error.core.InvalidLambdaParameterNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("invalidLambdaParameterName"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorInvalidLetBindingName(hydra.phantoms.TTerm<hydra.error.core.InvalidLetBindingNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("invalidLetBindingName"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorInvalidTypeLambdaParameterName(hydra.phantoms.TTerm<hydra.error.core.InvalidTypeLambdaParameterNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("invalidTypeLambdaParameterName"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorNestedTermAnnotation(hydra.phantoms.TTerm<hydra.error.core.NestedTermAnnotationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("nestedTermAnnotation"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorRedundantWrapUnwrap(hydra.phantoms.TTerm<hydra.error.core.RedundantWrapUnwrapError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("redundantWrapUnwrap"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorSelfApplication(hydra.phantoms.TTerm<hydra.error.core.SelfApplicationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("selfApplication"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorTermVariableShadowing(hydra.phantoms.TTerm<hydra.error.core.TermVariableShadowingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("termVariableShadowing"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorTypeVariableShadowingInTypeLambda(hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInTypeLambdaError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("typeVariableShadowingInTypeLambda"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorUndefinedTermVariable(hydra.phantoms.TTerm<hydra.error.core.UndefinedTermVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("undefinedTermVariable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorUndefinedTypeVariableInBindingType(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInBindingTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("undefinedTypeVariableInBindingType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorUndefinedTypeVariableInLambdaDomain(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInLambdaDomainError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("undefinedTypeVariableInLambdaDomain"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorUndefinedTypeVariableInTypeApplication(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInTypeApplicationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("undefinedTypeVariableInTypeApplication"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorUnknownPrimitiveName(hydra.phantoms.TTerm<hydra.error.core.UnknownPrimitiveNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("unknownPrimitiveName"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorUnnecessaryIdentityApplication(hydra.phantoms.TTerm<hydra.error.core.UnnecessaryIdentityApplicationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("unnecessaryIdentityApplication"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTermError> invalidTermErrorUntypedTermVariable(hydra.phantoms.TTerm<hydra.error.core.UntypedTermVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("untypedTermVariable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorDuplicateRecordTypeFieldNames(hydra.phantoms.TTerm<hydra.error.core.DuplicateRecordTypeFieldNamesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("duplicateRecordTypeFieldNames"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorDuplicateUnionTypeFieldNames(hydra.phantoms.TTerm<hydra.error.core.DuplicateUnionTypeFieldNamesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("duplicateUnionTypeFieldNames"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorEmptyRecordType(hydra.phantoms.TTerm<hydra.error.core.EmptyRecordTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("emptyRecordType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorEmptyTypeAnnotation(hydra.phantoms.TTerm<hydra.error.core.EmptyTypeAnnotationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("emptyTypeAnnotation"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorEmptyUnionType(hydra.phantoms.TTerm<hydra.error.core.EmptyUnionTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("emptyUnionType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorInvalidForallParameterName(hydra.phantoms.TTerm<hydra.error.core.InvalidForallParameterNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("invalidForallParameterName"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorInvalidTypeSchemeVariableName(hydra.phantoms.TTerm<hydra.error.core.InvalidTypeSchemeVariableNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("invalidTypeSchemeVariableName"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorNestedTypeAnnotation(hydra.phantoms.TTerm<hydra.error.core.NestedTypeAnnotationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("nestedTypeAnnotation"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorNonComparableMapKeyType(hydra.phantoms.TTerm<hydra.error.core.NonComparableMapKeyTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("nonComparableMapKeyType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorNonComparableSetElementType(hydra.phantoms.TTerm<hydra.error.core.NonComparableSetElementTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("nonComparableSetElementType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorSingleVariantUnion(hydra.phantoms.TTerm<hydra.error.core.SingleVariantUnionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("singleVariantUnion"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorTypeVariableShadowingInForall(hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInForallError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("typeVariableShadowingInForall"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorUndefinedTypeVariable(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("undefinedTypeVariable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeError> invalidTypeErrorVoidInNonBottomPosition(hydra.phantoms.TTerm<hydra.error.core.VoidInNonBottomPositionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("voidInNonBottomPosition"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeLambdaParameterNameError> invalidTypeLambdaParameterNameError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> invalidTypeLambdaParameterNameErrorLocation(hydra.phantoms.TTerm<hydra.error.core.InvalidTypeLambdaParameterNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> invalidTypeLambdaParameterNameErrorName(hydra.phantoms.TTerm<hydra.error.core.InvalidTypeLambdaParameterNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeLambdaParameterNameError> invalidTypeLambdaParameterNameErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.InvalidTypeLambdaParameterNameError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeLambdaParameterNameError> invalidTypeLambdaParameterNameErrorWithName(hydra.phantoms.TTerm<hydra.error.core.InvalidTypeLambdaParameterNameError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeSchemeVariableNameError> invalidTypeSchemeVariableNameError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> invalidTypeSchemeVariableNameErrorLocation(hydra.phantoms.TTerm<hydra.error.core.InvalidTypeSchemeVariableNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> invalidTypeSchemeVariableNameErrorName(hydra.phantoms.TTerm<hydra.error.core.InvalidTypeSchemeVariableNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeSchemeVariableNameError> invalidTypeSchemeVariableNameErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.InvalidTypeSchemeVariableNameError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.InvalidTypeSchemeVariableNameError> invalidTypeSchemeVariableNameErrorWithName(hydra.phantoms.TTerm<hydra.error.core.InvalidTypeSchemeVariableNameError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.NestedTermAnnotationError> nestedTermAnnotationError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NestedTermAnnotationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (location).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> nestedTermAnnotationErrorLocation(hydra.phantoms.TTerm<hydra.error.core.NestedTermAnnotationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.NestedTermAnnotationError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.NestedTermAnnotationError> nestedTermAnnotationErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.NestedTermAnnotationError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NestedTermAnnotationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.NestedTypeAnnotationError> nestedTypeAnnotationError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NestedTypeAnnotationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (location).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> nestedTypeAnnotationErrorLocation(hydra.phantoms.TTerm<hydra.error.core.NestedTypeAnnotationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.NestedTypeAnnotationError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.NestedTypeAnnotationError> nestedTypeAnnotationErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.NestedTypeAnnotationError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NestedTypeAnnotationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.NonComparableMapKeyTypeError> nonComparableMapKeyTypeError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Type> keyType) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("keyType"), (keyType).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> nonComparableMapKeyTypeErrorKeyType(hydra.phantoms.TTerm<hydra.error.core.NonComparableMapKeyTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), new hydra.core.Name("keyType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> nonComparableMapKeyTypeErrorLocation(hydra.phantoms.TTerm<hydra.error.core.NonComparableMapKeyTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.NonComparableMapKeyTypeError> nonComparableMapKeyTypeErrorWithKeyType(hydra.phantoms.TTerm<hydra.error.core.NonComparableMapKeyTypeError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("keyType"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.NonComparableMapKeyTypeError> nonComparableMapKeyTypeErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.NonComparableMapKeyTypeError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("keyType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), new hydra.core.Name("keyType"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.NonComparableSetElementTypeError> nonComparableSetElementTypeError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Type> elementType) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("elementType"), (elementType).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> nonComparableSetElementTypeErrorElementType(hydra.phantoms.TTerm<hydra.error.core.NonComparableSetElementTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), new hydra.core.Name("elementType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> nonComparableSetElementTypeErrorLocation(hydra.phantoms.TTerm<hydra.error.core.NonComparableSetElementTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.NonComparableSetElementTypeError> nonComparableSetElementTypeErrorWithElementType(hydra.phantoms.TTerm<hydra.error.core.NonComparableSetElementTypeError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("elementType"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.NonComparableSetElementTypeError> nonComparableSetElementTypeErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.NonComparableSetElementTypeError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("elementType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), new hydra.core.Name("elementType"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.RedundantWrapUnwrapError> redundantWrapUnwrapError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> typeName) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("typeName"), (typeName).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> redundantWrapUnwrapErrorLocation(hydra.phantoms.TTerm<hydra.error.core.RedundantWrapUnwrapError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> redundantWrapUnwrapErrorTypeName(hydra.phantoms.TTerm<hydra.error.core.RedundantWrapUnwrapError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), new hydra.core.Name("typeName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.RedundantWrapUnwrapError> redundantWrapUnwrapErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.RedundantWrapUnwrapError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), new hydra.core.Name("typeName"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.RedundantWrapUnwrapError> redundantWrapUnwrapErrorWithTypeName(hydra.phantoms.TTerm<hydra.error.core.RedundantWrapUnwrapError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeName"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.SelfApplicationError> selfApplicationError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.SelfApplicationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> selfApplicationErrorLocation(hydra.phantoms.TTerm<hydra.error.core.SelfApplicationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.SelfApplicationError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> selfApplicationErrorName(hydra.phantoms.TTerm<hydra.error.core.SelfApplicationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.SelfApplicationError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.SelfApplicationError> selfApplicationErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.SelfApplicationError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.SelfApplicationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.SelfApplicationError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.SelfApplicationError> selfApplicationErrorWithName(hydra.phantoms.TTerm<hydra.error.core.SelfApplicationError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.SelfApplicationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.SelfApplicationError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.SingleVariantUnionError> singleVariantUnionError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> fieldName) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.SingleVariantUnionError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("fieldName"), (fieldName).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> singleVariantUnionErrorFieldName(hydra.phantoms.TTerm<hydra.error.core.SingleVariantUnionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.SingleVariantUnionError"), new hydra.core.Name("fieldName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> singleVariantUnionErrorLocation(hydra.phantoms.TTerm<hydra.error.core.SingleVariantUnionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.SingleVariantUnionError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.SingleVariantUnionError> singleVariantUnionErrorWithFieldName(hydra.phantoms.TTerm<hydra.error.core.SingleVariantUnionError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.SingleVariantUnionError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.SingleVariantUnionError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fieldName"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.SingleVariantUnionError> singleVariantUnionErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.SingleVariantUnionError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.SingleVariantUnionError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("fieldName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.SingleVariantUnionError"), new hydra.core.Name("fieldName"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.TermVariableShadowingError> termVariableShadowingError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TermVariableShadowingError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> termVariableShadowingErrorLocation(hydra.phantoms.TTerm<hydra.error.core.TermVariableShadowingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TermVariableShadowingError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> termVariableShadowingErrorName(hydra.phantoms.TTerm<hydra.error.core.TermVariableShadowingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TermVariableShadowingError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.TermVariableShadowingError> termVariableShadowingErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.TermVariableShadowingError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TermVariableShadowingError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TermVariableShadowingError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.TermVariableShadowingError> termVariableShadowingErrorWithName(hydra.phantoms.TTerm<hydra.error.core.TermVariableShadowingError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TermVariableShadowingError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TermVariableShadowingError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInForallError> typeVariableShadowingInForallError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> typeVariableShadowingInForallErrorLocation(hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInForallError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> typeVariableShadowingInForallErrorName(hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInForallError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInForallError> typeVariableShadowingInForallErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInForallError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInForallError> typeVariableShadowingInForallErrorWithName(hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInForallError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInTypeLambdaError> typeVariableShadowingInTypeLambdaError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> typeVariableShadowingInTypeLambdaErrorLocation(hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInTypeLambdaError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> typeVariableShadowingInTypeLambdaErrorName(hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInTypeLambdaError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInTypeLambdaError> typeVariableShadowingInTypeLambdaErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInTypeLambdaError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInTypeLambdaError> typeVariableShadowingInTypeLambdaErrorWithName(hydra.phantoms.TTerm<hydra.error.core.TypeVariableShadowingInTypeLambdaError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
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

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTermVariableError> undefinedTermVariableError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> undefinedTermVariableErrorLocation(hydra.phantoms.TTerm<hydra.error.core.UndefinedTermVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedTermVariableErrorName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTermVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTermVariableError> undefinedTermVariableErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.UndefinedTermVariableError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTermVariableError> undefinedTermVariableErrorWithName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTermVariableError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableError> undefinedTypeVariableError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> undefinedTypeVariableErrorLocation(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedTypeVariableErrorName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableError> undefinedTypeVariableErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableError> undefinedTypeVariableErrorWithName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInBindingTypeError> undefinedTypeVariableInBindingTypeError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> undefinedTypeVariableInBindingTypeErrorLocation(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInBindingTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedTypeVariableInBindingTypeErrorName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInBindingTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInBindingTypeError> undefinedTypeVariableInBindingTypeErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInBindingTypeError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInBindingTypeError> undefinedTypeVariableInBindingTypeErrorWithName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInBindingTypeError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInLambdaDomainError> undefinedTypeVariableInLambdaDomainError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> undefinedTypeVariableInLambdaDomainErrorLocation(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInLambdaDomainError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedTypeVariableInLambdaDomainErrorName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInLambdaDomainError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInLambdaDomainError> undefinedTypeVariableInLambdaDomainErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInLambdaDomainError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInLambdaDomainError> undefinedTypeVariableInLambdaDomainErrorWithName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInLambdaDomainError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInTypeApplicationError> undefinedTypeVariableInTypeApplicationError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> undefinedTypeVariableInTypeApplicationErrorLocation(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInTypeApplicationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedTypeVariableInTypeApplicationErrorName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInTypeApplicationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInTypeApplicationError> undefinedTypeVariableInTypeApplicationErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInTypeApplicationError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInTypeApplicationError> undefinedTypeVariableInTypeApplicationErrorWithName(hydra.phantoms.TTerm<hydra.error.core.UndefinedTypeVariableInTypeApplicationError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> unexpectedTermVariantError(hydra.phantoms.TTerm<hydra.variants.TermVariant> expectedVariant, hydra.phantoms.TTerm<hydra.core.Term> actualTerm) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (expectedVariant).value),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), (actualTerm).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> unexpectedTermVariantErrorActualTerm(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), new hydra.core.Name("actualTerm"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> unexpectedTermVariantErrorExpectedVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), new hydra.core.Name("expectedVariant"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> unexpectedTermVariantErrorWithActualTerm(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), new hydra.core.Name("expectedVariant"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> unexpectedTermVariantErrorWithExpectedVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> original, hydra.phantoms.TTerm<hydra.variants.TermVariant> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), new hydra.core.Name("actualTerm"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> unexpectedTypeVariantError(hydra.phantoms.TTerm<hydra.variants.TypeVariant> expectedVariant, hydra.phantoms.TTerm<hydra.core.Type> actualType) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (expectedVariant).value),
      new hydra.core.Field(new hydra.core.Name("actualType"), (actualType).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unexpectedTypeVariantErrorActualType(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), new hydra.core.Name("actualType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> unexpectedTypeVariantErrorExpectedVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), new hydra.core.Name("expectedVariant"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> unexpectedTypeVariantErrorWithActualType(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), new hydra.core.Name("expectedVariant"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualType"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> unexpectedTypeVariantErrorWithExpectedVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> original, hydra.phantoms.TTerm<hydra.variants.TypeVariant> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actualType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), new hydra.core.Name("actualType"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnknownPrimitiveNameError> unknownPrimitiveNameError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> unknownPrimitiveNameErrorLocation(hydra.phantoms.TTerm<hydra.error.core.UnknownPrimitiveNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> unknownPrimitiveNameErrorName(hydra.phantoms.TTerm<hydra.error.core.UnknownPrimitiveNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnknownPrimitiveNameError> unknownPrimitiveNameErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.UnknownPrimitiveNameError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnknownPrimitiveNameError> unknownPrimitiveNameErrorWithName(hydra.phantoms.TTerm<hydra.error.core.UnknownPrimitiveNameError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnnecessaryIdentityApplicationError> unnecessaryIdentityApplicationError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnnecessaryIdentityApplicationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (location).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> unnecessaryIdentityApplicationErrorLocation(hydra.phantoms.TTerm<hydra.error.core.UnnecessaryIdentityApplicationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UnnecessaryIdentityApplicationError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UnnecessaryIdentityApplicationError> unnecessaryIdentityApplicationErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.UnnecessaryIdentityApplicationError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnnecessaryIdentityApplicationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UntypedTermVariableError> untypedTermVariableError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UntypedTermVariableError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (location).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> untypedTermVariableErrorLocation(hydra.phantoms.TTerm<hydra.error.core.UntypedTermVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UntypedTermVariableError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> untypedTermVariableErrorName(hydra.phantoms.TTerm<hydra.error.core.UntypedTermVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UntypedTermVariableError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UntypedTermVariableError> untypedTermVariableErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.UntypedTermVariableError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UntypedTermVariableError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UntypedTermVariableError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.UntypedTermVariableError> untypedTermVariableErrorWithName(hydra.phantoms.TTerm<hydra.error.core.UntypedTermVariableError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UntypedTermVariableError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.UntypedTermVariableError"), new hydra.core.Name("location"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.core.VoidInNonBottomPositionError> voidInNonBottomPositionError(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> location) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.VoidInNonBottomPositionError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (location).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> voidInNonBottomPositionErrorLocation(hydra.phantoms.TTerm<hydra.error.core.VoidInNonBottomPositionError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.core.VoidInNonBottomPositionError"), new hydra.core.Name("location"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.core.VoidInNonBottomPositionError> voidInNonBottomPositionErrorWithLocation(hydra.phantoms.TTerm<hydra.error.core.VoidInNonBottomPositionError> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.VoidInNonBottomPositionError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("location"), (newVal).value)))));
  }
}
