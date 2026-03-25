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
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("checking"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorDecoding(hydra.phantoms.TTerm<hydra.errors.DecodingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("decoding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorDuplicateBinding(hydra.phantoms.TTerm<hydra.error.core.DuplicateBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("duplicateBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorDuplicateField(hydra.phantoms.TTerm<hydra.error.core.DuplicateFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("duplicateField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorOther(hydra.phantoms.TTerm<hydra.errors.OtherError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("other"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUndefinedField(hydra.phantoms.TTerm<hydra.error.core.UndefinedFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("undefinedField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUndefinedTermVariable(hydra.phantoms.TTerm<hydra.error.core.UndefinedTermVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("undefinedTermVariable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUnexpectedTermVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("unexpectedTermVariant"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUnexpectedTypeVariant(hydra.phantoms.TTerm<hydra.error.core.UnexpectedTypeVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("unexpectedTypeVariant"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUnification(hydra.phantoms.TTerm<hydra.errors.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("unification"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.Error_> errorUntypedTermVariable(hydra.phantoms.TTerm<hydra.error.core.UntypedTermVariableError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("untypedTermVariable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.errors.OtherError> otherError(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.OtherError"), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unDecodingError(hydra.phantoms.TTerm<hydra.errors.DecodingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.errors.DecodingError")))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unOtherError(hydra.phantoms.TTerm<hydra.errors.OtherError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.errors.OtherError")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationError> unificationError(hydra.phantoms.TTerm<hydra.core.Type> leftType, hydra.phantoms.TTerm<hydra.core.Type> rightType, hydra.phantoms.TTerm<String> message) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("leftType"), (leftType).value),
      new hydra.core.Field(new hydra.core.Name("rightType"), (rightType).value),
      new hydra.core.Field(new hydra.core.Name("message"), (message).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unificationErrorLeftType(hydra.phantoms.TTerm<hydra.errors.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("leftType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unificationErrorMessage(hydra.phantoms.TTerm<hydra.errors.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("message"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unificationErrorRightType(hydra.phantoms.TTerm<hydra.errors.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("rightType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationError> unificationErrorWithLeftType(hydra.phantoms.TTerm<hydra.errors.UnificationError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("leftType"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rightType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("rightType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("message"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationError> unificationErrorWithMessage(hydra.phantoms.TTerm<hydra.errors.UnificationError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("leftType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("leftType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rightType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("rightType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("message"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.errors.UnificationError> unificationErrorWithRightType(hydra.phantoms.TTerm<hydra.errors.UnificationError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("leftType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("leftType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rightType"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.errors.UnificationError"), new hydra.core.Name("message"))))), (original).value)))))));
  }
}
