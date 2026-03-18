// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.error;

/**
 * DSL functions for hydra.error
 */
public interface Error_ {
  static hydra.phantoms.TTerm<hydra.error.CheckingError> checkingErrorIncorrectUnification(hydra.phantoms.TTerm<hydra.error.IncorrectUnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.CheckingError"), new hydra.core.Field(new hydra.core.Name("incorrectUnification"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.CheckingError> checkingErrorNotAForallType(hydra.phantoms.TTerm<hydra.error.NotAForallTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.CheckingError"), new hydra.core.Field(new hydra.core.Name("notAForallType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.CheckingError> checkingErrorNotAFunctionType(hydra.phantoms.TTerm<hydra.error.NotAFunctionTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.CheckingError"), new hydra.core.Field(new hydra.core.Name("notAFunctionType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.CheckingError> checkingErrorTypeArityMismatch(hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.CheckingError"), new hydra.core.Field(new hydra.core.Name("typeArityMismatch"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.CheckingError> checkingErrorTypeMismatch(hydra.phantoms.TTerm<hydra.error.TypeMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.CheckingError"), new hydra.core.Field(new hydra.core.Name("typeMismatch"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.CheckingError> checkingErrorUnboundTypeVariables(hydra.phantoms.TTerm<hydra.error.UnboundTypeVariablesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.CheckingError"), new hydra.core.Field(new hydra.core.Name("unboundTypeVariables"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.CheckingError> checkingErrorUnequalTypes(hydra.phantoms.TTerm<hydra.error.UnequalTypesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.CheckingError"), new hydra.core.Field(new hydra.core.Name("unequalTypes"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.CheckingError> checkingErrorUnsupportedTermVariant(hydra.phantoms.TTerm<hydra.error.UnsupportedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.CheckingError"), new hydra.core.Field(new hydra.core.Name("unsupportedTermVariant"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.CheckingError> checkingErrorUntypedLambda(hydra.phantoms.TTerm<hydra.error.UntypedLambdaError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.CheckingError"), new hydra.core.Field(new hydra.core.Name("untypedLambda"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.CheckingError> checkingErrorUntypedLetBinding(hydra.phantoms.TTerm<hydra.error.UntypedLetBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.CheckingError"), new hydra.core.Field(new hydra.core.Name("untypedLetBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.DecodingError> decodingError(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.error.DecodingError"), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unDecodingError(hydra.phantoms.TTerm<hydra.error.DecodingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.error.DecodingError")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.DuplicateBindingError> duplicateBindingError(hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.DuplicateBindingError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> duplicateBindingErrorName(hydra.phantoms.TTerm<hydra.error.DuplicateBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.DuplicateBindingError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.DuplicateBindingError> duplicateBindingErrorWithName(hydra.phantoms.TTerm<hydra.error.DuplicateBindingError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.DuplicateBindingError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.DuplicateFieldError> duplicateFieldError(hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.DuplicateFieldError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> duplicateFieldErrorName(hydra.phantoms.TTerm<hydra.error.DuplicateFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.DuplicateFieldError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.DuplicateFieldError> duplicateFieldErrorWithName(hydra.phantoms.TTerm<hydra.error.DuplicateFieldError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.DuplicateFieldError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorChecking(hydra.phantoms.TTerm<hydra.error.CheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("checking"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorDecoding(hydra.phantoms.TTerm<hydra.error.DecodingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("decoding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorDuplicateBinding(hydra.phantoms.TTerm<hydra.error.DuplicateBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("duplicateBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorDuplicateField(hydra.phantoms.TTerm<hydra.error.DuplicateFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("duplicateField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorOther(hydra.phantoms.TTerm<hydra.error.OtherError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("other"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorUndefinedField(hydra.phantoms.TTerm<hydra.error.UndefinedFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("undefinedField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorUndefinedTerm(hydra.phantoms.TTerm<hydra.error.UndefinedTermError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("undefinedTerm"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorUndefinedType(hydra.phantoms.TTerm<hydra.error.UndefinedTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("undefinedType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorUnexpectedTermVariant(hydra.phantoms.TTerm<hydra.error.UnexpectedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("unexpectedTermVariant"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorUnexpectedTypeVariant(hydra.phantoms.TTerm<hydra.error.UnexpectedTypeVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("unexpectedTypeVariant"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.Error_> errorUnification(hydra.phantoms.TTerm<hydra.error.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.Error"), new hydra.core.Field(new hydra.core.Name("unification"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.IncorrectUnificationError> incorrectUnificationError(hydra.phantoms.TTerm<hydra.typing.TypeSubst> substitution) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.IncorrectUnificationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("substitution"), (substitution).value)))));
  }

  static hydra.phantoms.TTerm<hydra.typing.TypeSubst> incorrectUnificationErrorSubstitution(hydra.phantoms.TTerm<hydra.error.IncorrectUnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.IncorrectUnificationError"), new hydra.core.Name("substitution"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.IncorrectUnificationError> incorrectUnificationErrorWithSubstitution(hydra.phantoms.TTerm<hydra.error.IncorrectUnificationError> original, hydra.phantoms.TTerm<hydra.typing.TypeSubst> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.IncorrectUnificationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("substitution"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.NotAForallTypeError> notAForallTypeError(hydra.phantoms.TTerm<hydra.core.Type> type, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Type>> typeArguments) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.NotAForallTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), (typeArguments).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> notAForallTypeErrorType(hydra.phantoms.TTerm<hydra.error.NotAForallTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.NotAForallTypeError"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Type>> notAForallTypeErrorTypeArguments(hydra.phantoms.TTerm<hydra.error.NotAForallTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.NotAForallTypeError"), new hydra.core.Name("typeArguments"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.NotAForallTypeError> notAForallTypeErrorWithType(hydra.phantoms.TTerm<hydra.error.NotAForallTypeError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.NotAForallTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.NotAForallTypeError"), new hydra.core.Name("typeArguments"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.NotAForallTypeError> notAForallTypeErrorWithTypeArguments(hydra.phantoms.TTerm<hydra.error.NotAForallTypeError> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.NotAForallTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.NotAForallTypeError"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.NotAFunctionTypeError> notAFunctionTypeError(hydra.phantoms.TTerm<hydra.core.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.NotAFunctionTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> notAFunctionTypeErrorType(hydra.phantoms.TTerm<hydra.error.NotAFunctionTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.NotAFunctionTypeError"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.NotAFunctionTypeError> notAFunctionTypeErrorWithType(hydra.phantoms.TTerm<hydra.error.NotAFunctionTypeError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.NotAFunctionTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.OtherError> otherError(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.error.OtherError"), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unOtherError(hydra.phantoms.TTerm<hydra.error.OtherError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.error.OtherError")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> typeArityMismatchError(hydra.phantoms.TTerm<hydra.core.Type> type, hydra.phantoms.TTerm<Integer> expectedArity, hydra.phantoms.TTerm<Integer> actualArity, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Type>> typeArguments) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), (expectedArity).value),
      new hydra.core.Field(new hydra.core.Name("actualArity"), (actualArity).value),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), (typeArguments).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeArityMismatchErrorType(hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<Integer> typeArityMismatchErrorExpectedArity(hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("expectedArity"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<Integer> typeArityMismatchErrorActualArity(hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("actualArity"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Type>> typeArityMismatchErrorTypeArguments(hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("typeArguments"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> typeArityMismatchErrorWithType(hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("expectedArity"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("actualArity"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("typeArguments"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> typeArityMismatchErrorWithExpectedArity(hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> original, hydra.phantoms.TTerm<Integer> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actualArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("actualArity"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("typeArguments"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> typeArityMismatchErrorWithActualArity(hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> original, hydra.phantoms.TTerm<Integer> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("expectedArity"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualArity"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("typeArguments"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> typeArityMismatchErrorWithTypeArguments(hydra.phantoms.TTerm<hydra.error.TypeArityMismatchError> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("expectedArity"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeArityMismatchError"), new hydra.core.Name("actualArity"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.TypeMismatchError> typeMismatchError(hydra.phantoms.TTerm<hydra.core.Type> expectedType, hydra.phantoms.TTerm<hydra.core.Type> actualType) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.TypeMismatchError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedType"), (expectedType).value),
      new hydra.core.Field(new hydra.core.Name("actualType"), (actualType).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeMismatchErrorExpectedType(hydra.phantoms.TTerm<hydra.error.TypeMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeMismatchError"), new hydra.core.Name("expectedType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeMismatchErrorActualType(hydra.phantoms.TTerm<hydra.error.TypeMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeMismatchError"), new hydra.core.Name("actualType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.TypeMismatchError> typeMismatchErrorWithExpectedType(hydra.phantoms.TTerm<hydra.error.TypeMismatchError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.TypeMismatchError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedType"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actualType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeMismatchError"), new hydra.core.Name("actualType"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.TypeMismatchError> typeMismatchErrorWithActualType(hydra.phantoms.TTerm<hydra.error.TypeMismatchError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.TypeMismatchError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.TypeMismatchError"), new hydra.core.Name("expectedType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualType"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnboundTypeVariablesError> unboundTypeVariablesError(hydra.phantoms.TTerm<hydra.util.PersistentSet<hydra.core.Name>> variables, hydra.phantoms.TTerm<hydra.core.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnboundTypeVariablesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("variables"), (variables).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.PersistentSet<hydra.core.Name>> unboundTypeVariablesErrorVariables(hydra.phantoms.TTerm<hydra.error.UnboundTypeVariablesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnboundTypeVariablesError"), new hydra.core.Name("variables"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unboundTypeVariablesErrorType(hydra.phantoms.TTerm<hydra.error.UnboundTypeVariablesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnboundTypeVariablesError"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.UnboundTypeVariablesError> unboundTypeVariablesErrorWithVariables(hydra.phantoms.TTerm<hydra.error.UnboundTypeVariablesError> original, hydra.phantoms.TTerm<hydra.util.PersistentSet<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnboundTypeVariablesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("variables"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnboundTypeVariablesError"), new hydra.core.Name("type"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnboundTypeVariablesError> unboundTypeVariablesErrorWithType(hydra.phantoms.TTerm<hydra.error.UnboundTypeVariablesError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnboundTypeVariablesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("variables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnboundTypeVariablesError"), new hydra.core.Name("variables"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.UndefinedFieldError> undefinedFieldError(hydra.phantoms.TTerm<hydra.core.Name> fieldName, hydra.phantoms.TTerm<hydra.core.Name> typeName) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UndefinedFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fieldName"), (fieldName).value),
      new hydra.core.Field(new hydra.core.Name("typeName"), (typeName).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedFieldErrorFieldName(hydra.phantoms.TTerm<hydra.error.UndefinedFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UndefinedFieldError"), new hydra.core.Name("fieldName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedFieldErrorTypeName(hydra.phantoms.TTerm<hydra.error.UndefinedFieldError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UndefinedFieldError"), new hydra.core.Name("typeName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.UndefinedFieldError> undefinedFieldErrorWithFieldName(hydra.phantoms.TTerm<hydra.error.UndefinedFieldError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UndefinedFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fieldName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UndefinedFieldError"), new hydra.core.Name("typeName"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.UndefinedFieldError> undefinedFieldErrorWithTypeName(hydra.phantoms.TTerm<hydra.error.UndefinedFieldError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UndefinedFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fieldName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UndefinedFieldError"), new hydra.core.Name("fieldName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeName"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.UndefinedTermError> undefinedTermError(hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UndefinedTermError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedTermErrorName(hydra.phantoms.TTerm<hydra.error.UndefinedTermError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UndefinedTermError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.UndefinedTermError> undefinedTermErrorWithName(hydra.phantoms.TTerm<hydra.error.UndefinedTermError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UndefinedTermError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.UndefinedTypeError> undefinedTypeError(hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UndefinedTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedTypeErrorName(hydra.phantoms.TTerm<hydra.error.UndefinedTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UndefinedTypeError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.UndefinedTypeError> undefinedTypeErrorWithName(hydra.phantoms.TTerm<hydra.error.UndefinedTypeError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UndefinedTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnequalTypesError> unequalTypesError(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Type>> types, hydra.phantoms.TTerm<String> description) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnequalTypesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("types"), (types).value),
      new hydra.core.Field(new hydra.core.Name("description"), (description).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Type>> unequalTypesErrorTypes(hydra.phantoms.TTerm<hydra.error.UnequalTypesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnequalTypesError"), new hydra.core.Name("types"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unequalTypesErrorDescription(hydra.phantoms.TTerm<hydra.error.UnequalTypesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnequalTypesError"), new hydra.core.Name("description"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.UnequalTypesError> unequalTypesErrorWithTypes(hydra.phantoms.TTerm<hydra.error.UnequalTypesError> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnequalTypesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("types"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnequalTypesError"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnequalTypesError> unequalTypesErrorWithDescription(hydra.phantoms.TTerm<hydra.error.UnequalTypesError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnequalTypesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnequalTypesError"), new hydra.core.Name("types"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnexpectedTermVariantError> unexpectedTermVariantError(hydra.phantoms.TTerm<hydra.variants.TermVariant> expectedVariant, hydra.phantoms.TTerm<hydra.core.Term> actualTerm) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnexpectedTermVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (expectedVariant).value),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), (actualTerm).value)))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> unexpectedTermVariantErrorExpectedVariant(hydra.phantoms.TTerm<hydra.error.UnexpectedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnexpectedTermVariantError"), new hydra.core.Name("expectedVariant"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> unexpectedTermVariantErrorActualTerm(hydra.phantoms.TTerm<hydra.error.UnexpectedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnexpectedTermVariantError"), new hydra.core.Name("actualTerm"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.UnexpectedTermVariantError> unexpectedTermVariantErrorWithExpectedVariant(hydra.phantoms.TTerm<hydra.error.UnexpectedTermVariantError> original, hydra.phantoms.TTerm<hydra.variants.TermVariant> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnexpectedTermVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnexpectedTermVariantError"), new hydra.core.Name("actualTerm"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnexpectedTermVariantError> unexpectedTermVariantErrorWithActualTerm(hydra.phantoms.TTerm<hydra.error.UnexpectedTermVariantError> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnexpectedTermVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnexpectedTermVariantError"), new hydra.core.Name("expectedVariant"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnexpectedTypeVariantError> unexpectedTypeVariantError(hydra.phantoms.TTerm<hydra.variants.TypeVariant> expectedVariant, hydra.phantoms.TTerm<hydra.core.Type> actualType) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (expectedVariant).value),
      new hydra.core.Field(new hydra.core.Name("actualType"), (actualType).value)))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TypeVariant> unexpectedTypeVariantErrorExpectedVariant(hydra.phantoms.TTerm<hydra.error.UnexpectedTypeVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), new hydra.core.Name("expectedVariant"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unexpectedTypeVariantErrorActualType(hydra.phantoms.TTerm<hydra.error.UnexpectedTypeVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), new hydra.core.Name("actualType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.UnexpectedTypeVariantError> unexpectedTypeVariantErrorWithExpectedVariant(hydra.phantoms.TTerm<hydra.error.UnexpectedTypeVariantError> original, hydra.phantoms.TTerm<hydra.variants.TypeVariant> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actualType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), new hydra.core.Name("actualType"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnexpectedTypeVariantError> unexpectedTypeVariantErrorWithActualType(hydra.phantoms.TTerm<hydra.error.UnexpectedTypeVariantError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), new hydra.core.Name("expectedVariant"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualType"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnificationError> unificationError(hydra.phantoms.TTerm<hydra.core.Type> leftType, hydra.phantoms.TTerm<hydra.core.Type> rightType, hydra.phantoms.TTerm<String> message) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnificationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("leftType"), (leftType).value),
      new hydra.core.Field(new hydra.core.Name("rightType"), (rightType).value),
      new hydra.core.Field(new hydra.core.Name("message"), (message).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unificationErrorLeftType(hydra.phantoms.TTerm<hydra.error.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnificationError"), new hydra.core.Name("leftType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unificationErrorRightType(hydra.phantoms.TTerm<hydra.error.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnificationError"), new hydra.core.Name("rightType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unificationErrorMessage(hydra.phantoms.TTerm<hydra.error.UnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnificationError"), new hydra.core.Name("message"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.UnificationError> unificationErrorWithLeftType(hydra.phantoms.TTerm<hydra.error.UnificationError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnificationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("leftType"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rightType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnificationError"), new hydra.core.Name("rightType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnificationError"), new hydra.core.Name("message"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnificationError> unificationErrorWithRightType(hydra.phantoms.TTerm<hydra.error.UnificationError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnificationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("leftType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnificationError"), new hydra.core.Name("leftType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rightType"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnificationError"), new hydra.core.Name("message"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnificationError> unificationErrorWithMessage(hydra.phantoms.TTerm<hydra.error.UnificationError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnificationError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("leftType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnificationError"), new hydra.core.Name("leftType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rightType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnificationError"), new hydra.core.Name("rightType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("message"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.UnsupportedTermVariantError> unsupportedTermVariantError(hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariant) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnsupportedTermVariantError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("termVariant"), (termVariant).value)))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> unsupportedTermVariantErrorTermVariant(hydra.phantoms.TTerm<hydra.error.UnsupportedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UnsupportedTermVariantError"), new hydra.core.Name("termVariant"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.UnsupportedTermVariantError> unsupportedTermVariantErrorWithTermVariant(hydra.phantoms.TTerm<hydra.error.UnsupportedTermVariantError> original, hydra.phantoms.TTerm<hydra.variants.TermVariant> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UnsupportedTermVariantError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("termVariant"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.UntypedLambdaError> untypedLambdaError() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UntypedLambdaError"), hydra.util.ConsList.empty())));
  }

  static hydra.phantoms.TTerm<hydra.error.UntypedLetBindingError> untypedLetBindingError(hydra.phantoms.TTerm<hydra.core.Binding> binding) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UntypedLetBindingError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("binding"), (binding).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Binding> untypedLetBindingErrorBinding(hydra.phantoms.TTerm<hydra.error.UntypedLetBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.UntypedLetBindingError"), new hydra.core.Name("binding"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.UntypedLetBindingError> untypedLetBindingErrorWithBinding(hydra.phantoms.TTerm<hydra.error.UntypedLetBindingError> original, hydra.phantoms.TTerm<hydra.core.Binding> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.UntypedLetBindingError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("binding"), (newVal).value)))));
  }
}
