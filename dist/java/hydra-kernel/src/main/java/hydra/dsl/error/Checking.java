// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.error;

/**
 * DSL functions for hydra.error.checking
 */
public interface Checking {
  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorIncorrectUnification(hydra.phantoms.TTerm<hydra.error.checking.IncorrectUnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("incorrectUnification"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorNotAForallType(hydra.phantoms.TTerm<hydra.error.checking.NotAForallTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("notAForallType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorNotAFunctionType(hydra.phantoms.TTerm<hydra.error.checking.NotAFunctionTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("notAFunctionType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorOther(hydra.phantoms.TTerm<hydra.error.checking.OtherCheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("other"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorTypeArityMismatch(hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("typeArityMismatch"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorTypeMismatch(hydra.phantoms.TTerm<hydra.error.checking.TypeMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("typeMismatch"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorUnboundTypeVariables(hydra.phantoms.TTerm<hydra.error.checking.UnboundTypeVariablesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("unboundTypeVariables"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorUndefinedTermVariable(hydra.phantoms.TTerm<hydra.error.checking.UndefinedTermVariableCheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("undefinedTermVariable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorUnequalTypes(hydra.phantoms.TTerm<hydra.error.checking.UnequalTypesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("unequalTypes"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorUnsupportedTermVariant(hydra.phantoms.TTerm<hydra.error.checking.UnsupportedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("unsupportedTermVariant"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorUntypedLambda(hydra.phantoms.TTerm<hydra.error.checking.UntypedLambdaError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("untypedLambda"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorUntypedLetBinding(hydra.phantoms.TTerm<hydra.error.checking.UntypedLetBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("untypedLetBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.CheckingError> checkingErrorUntypedTermVariable(hydra.phantoms.TTerm<hydra.error.checking.UntypedTermVariableCheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("untypedTermVariable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.IncorrectUnificationError> incorrectUnificationError(hydra.phantoms.TTerm<hydra.typing.TypeSubst> substitution) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.IncorrectUnificationError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("substitution"), (substitution).value)))));
  }

  static hydra.phantoms.TTerm<hydra.typing.TypeSubst> incorrectUnificationErrorSubstitution(hydra.phantoms.TTerm<hydra.error.checking.IncorrectUnificationError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.IncorrectUnificationError"), new hydra.core.Name("substitution"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.IncorrectUnificationError> incorrectUnificationErrorWithSubstitution(hydra.phantoms.TTerm<hydra.error.checking.IncorrectUnificationError> original, hydra.phantoms.TTerm<hydra.typing.TypeSubst> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.IncorrectUnificationError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("substitution"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.NotAForallTypeError> notAForallTypeError(hydra.phantoms.TTerm<hydra.core.Type> type, hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> typeArguments) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.NotAForallTypeError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), (typeArguments).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> notAForallTypeErrorType(hydra.phantoms.TTerm<hydra.error.checking.NotAForallTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.NotAForallTypeError"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> notAForallTypeErrorTypeArguments(hydra.phantoms.TTerm<hydra.error.checking.NotAForallTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.NotAForallTypeError"), new hydra.core.Name("typeArguments"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.NotAForallTypeError> notAForallTypeErrorWithType(hydra.phantoms.TTerm<hydra.error.checking.NotAForallTypeError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.NotAForallTypeError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.NotAForallTypeError"), new hydra.core.Name("typeArguments"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.NotAForallTypeError> notAForallTypeErrorWithTypeArguments(hydra.phantoms.TTerm<hydra.error.checking.NotAForallTypeError> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.NotAForallTypeError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.NotAForallTypeError"), new hydra.core.Name("type"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.NotAFunctionTypeError> notAFunctionTypeError(hydra.phantoms.TTerm<hydra.core.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.NotAFunctionTypeError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> notAFunctionTypeErrorType(hydra.phantoms.TTerm<hydra.error.checking.NotAFunctionTypeError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.NotAFunctionTypeError"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.NotAFunctionTypeError> notAFunctionTypeErrorWithType(hydra.phantoms.TTerm<hydra.error.checking.NotAFunctionTypeError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.NotAFunctionTypeError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.OtherCheckingError> otherCheckingError(hydra.phantoms.TTerm<hydra.paths.SubtermPath> path, hydra.phantoms.TTerm<String> message) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.OtherCheckingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (path).value),
      new hydra.core.Field(new hydra.core.Name("message"), (message).value)))));
  }

  static hydra.phantoms.TTerm<String> otherCheckingErrorMessage(hydra.phantoms.TTerm<hydra.error.checking.OtherCheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.OtherCheckingError"), new hydra.core.Name("message"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermPath> otherCheckingErrorPath(hydra.phantoms.TTerm<hydra.error.checking.OtherCheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.OtherCheckingError"), new hydra.core.Name("path"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.OtherCheckingError> otherCheckingErrorWithMessage(hydra.phantoms.TTerm<hydra.error.checking.OtherCheckingError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.OtherCheckingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.OtherCheckingError"), new hydra.core.Name("path"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("message"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.OtherCheckingError> otherCheckingErrorWithPath(hydra.phantoms.TTerm<hydra.error.checking.OtherCheckingError> original, hydra.phantoms.TTerm<hydra.paths.SubtermPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.OtherCheckingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.OtherCheckingError"), new hydra.core.Name("message"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> typeArityMismatchError(hydra.phantoms.TTerm<hydra.core.Type> type, hydra.phantoms.TTerm<Integer> expectedArity, hydra.phantoms.TTerm<Integer> actualArity, hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> typeArguments) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), (expectedArity).value),
      new hydra.core.Field(new hydra.core.Name("actualArity"), (actualArity).value),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), (typeArguments).value)))));
  }

  static hydra.phantoms.TTerm<Integer> typeArityMismatchErrorActualArity(hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("actualArity"))), (x).value)));
  }

  static hydra.phantoms.TTerm<Integer> typeArityMismatchErrorExpectedArity(hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("expectedArity"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeArityMismatchErrorType(hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> typeArityMismatchErrorTypeArguments(hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("typeArguments"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> typeArityMismatchErrorWithActualArity(hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> original, hydra.phantoms.TTerm<Integer> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("type"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("expectedArity"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualArity"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("typeArguments"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> typeArityMismatchErrorWithExpectedArity(hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> original, hydra.phantoms.TTerm<Integer> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("type"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actualArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("actualArity"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("typeArguments"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> typeArityMismatchErrorWithType(hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("expectedArity"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("actualArity"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("typeArguments"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> typeArityMismatchErrorWithTypeArguments(hydra.phantoms.TTerm<hydra.error.checking.TypeArityMismatchError> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("type"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("expectedArity"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualArity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), new hydra.core.Name("actualArity"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.TypeMismatchError> typeMismatchError(hydra.phantoms.TTerm<hydra.core.Type> expectedType, hydra.phantoms.TTerm<hydra.core.Type> actualType) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.TypeMismatchError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("expectedType"), (expectedType).value),
      new hydra.core.Field(new hydra.core.Name("actualType"), (actualType).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeMismatchErrorActualType(hydra.phantoms.TTerm<hydra.error.checking.TypeMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeMismatchError"), new hydra.core.Name("actualType"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeMismatchErrorExpectedType(hydra.phantoms.TTerm<hydra.error.checking.TypeMismatchError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeMismatchError"), new hydra.core.Name("expectedType"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.TypeMismatchError> typeMismatchErrorWithActualType(hydra.phantoms.TTerm<hydra.error.checking.TypeMismatchError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.TypeMismatchError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("expectedType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeMismatchError"), new hydra.core.Name("expectedType"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("actualType"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.TypeMismatchError> typeMismatchErrorWithExpectedType(hydra.phantoms.TTerm<hydra.error.checking.TypeMismatchError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.TypeMismatchError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("expectedType"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("actualType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.TypeMismatchError"), new hydra.core.Name("actualType"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UnboundTypeVariablesError> unboundTypeVariablesError(hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> variables, hydra.phantoms.TTerm<hydra.core.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), (variables).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unboundTypeVariablesErrorType(hydra.phantoms.TTerm<hydra.error.checking.UnboundTypeVariablesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> unboundTypeVariablesErrorVariables(hydra.phantoms.TTerm<hydra.error.checking.UnboundTypeVariablesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), new hydra.core.Name("variables"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UnboundTypeVariablesError> unboundTypeVariablesErrorWithType(hydra.phantoms.TTerm<hydra.error.checking.UnboundTypeVariablesError> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), new hydra.core.Name("variables"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UnboundTypeVariablesError> unboundTypeVariablesErrorWithVariables(hydra.phantoms.TTerm<hydra.error.checking.UnboundTypeVariablesError> original, hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UndefinedTermVariableCheckingError> undefinedTermVariableCheckingError(hydra.phantoms.TTerm<hydra.paths.SubtermPath> path, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (path).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> undefinedTermVariableCheckingErrorName(hydra.phantoms.TTerm<hydra.error.checking.UndefinedTermVariableCheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermPath> undefinedTermVariableCheckingErrorPath(hydra.phantoms.TTerm<hydra.error.checking.UndefinedTermVariableCheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), new hydra.core.Name("path"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UndefinedTermVariableCheckingError> undefinedTermVariableCheckingErrorWithName(hydra.phantoms.TTerm<hydra.error.checking.UndefinedTermVariableCheckingError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), new hydra.core.Name("path"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UndefinedTermVariableCheckingError> undefinedTermVariableCheckingErrorWithPath(hydra.phantoms.TTerm<hydra.error.checking.UndefinedTermVariableCheckingError> original, hydra.phantoms.TTerm<hydra.paths.SubtermPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), new hydra.core.Name("name"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UnequalTypesError> unequalTypesError(hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> types, hydra.phantoms.TTerm<String> description) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnequalTypesError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("types"), (types).value),
      new hydra.core.Field(new hydra.core.Name("description"), (description).value)))));
  }

  static hydra.phantoms.TTerm<String> unequalTypesErrorDescription(hydra.phantoms.TTerm<hydra.error.checking.UnequalTypesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UnequalTypesError"), new hydra.core.Name("description"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> unequalTypesErrorTypes(hydra.phantoms.TTerm<hydra.error.checking.UnequalTypesError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UnequalTypesError"), new hydra.core.Name("types"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UnequalTypesError> unequalTypesErrorWithDescription(hydra.phantoms.TTerm<hydra.error.checking.UnequalTypesError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnequalTypesError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UnequalTypesError"), new hydra.core.Name("types"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UnequalTypesError> unequalTypesErrorWithTypes(hydra.phantoms.TTerm<hydra.error.checking.UnequalTypesError> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnequalTypesError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("types"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UnequalTypesError"), new hydra.core.Name("description"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UnsupportedTermVariantError> unsupportedTermVariantError(hydra.phantoms.TTerm<hydra.variants.TermVariant> termVariant) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnsupportedTermVariantError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("termVariant"), (termVariant).value)))));
  }

  static hydra.phantoms.TTerm<hydra.variants.TermVariant> unsupportedTermVariantErrorTermVariant(hydra.phantoms.TTerm<hydra.error.checking.UnsupportedTermVariantError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UnsupportedTermVariantError"), new hydra.core.Name("termVariant"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UnsupportedTermVariantError> unsupportedTermVariantErrorWithTermVariant(hydra.phantoms.TTerm<hydra.error.checking.UnsupportedTermVariantError> original, hydra.phantoms.TTerm<hydra.variants.TermVariant> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnsupportedTermVariantError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("termVariant"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UntypedLambdaError> untypedLambdaError() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UntypedLambdaError"), java.util.Collections.emptyList())));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UntypedLetBindingError> untypedLetBindingError(hydra.phantoms.TTerm<hydra.core.Binding> binding) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UntypedLetBindingError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("binding"), (binding).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Binding> untypedLetBindingErrorBinding(hydra.phantoms.TTerm<hydra.error.checking.UntypedLetBindingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UntypedLetBindingError"), new hydra.core.Name("binding"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UntypedLetBindingError> untypedLetBindingErrorWithBinding(hydra.phantoms.TTerm<hydra.error.checking.UntypedLetBindingError> original, hydra.phantoms.TTerm<hydra.core.Binding> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UntypedLetBindingError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("binding"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UntypedTermVariableCheckingError> untypedTermVariableCheckingError(hydra.phantoms.TTerm<hydra.paths.SubtermPath> path, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (path).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> untypedTermVariableCheckingErrorName(hydra.phantoms.TTerm<hydra.error.checking.UntypedTermVariableCheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermPath> untypedTermVariableCheckingErrorPath(hydra.phantoms.TTerm<hydra.error.checking.UntypedTermVariableCheckingError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), new hydra.core.Name("path"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UntypedTermVariableCheckingError> untypedTermVariableCheckingErrorWithName(hydra.phantoms.TTerm<hydra.error.checking.UntypedTermVariableCheckingError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), new hydra.core.Name("path"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.checking.UntypedTermVariableCheckingError> untypedTermVariableCheckingErrorWithPath(hydra.phantoms.TTerm<hydra.error.checking.UntypedTermVariableCheckingError> original, hydra.phantoms.TTerm<hydra.paths.SubtermPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), new hydra.core.Name("name"))), (original).value)))))));
  }
}
