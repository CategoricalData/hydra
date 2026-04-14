// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.parsing
 */
public interface Parsing {
  static hydra.phantoms.TTerm<hydra.parsing.ParseError> parseError(hydra.phantoms.TTerm<String> message, hydra.phantoms.TTerm<String> remainder) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.parsing.ParseError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("message"), (message).value),
      new hydra.core.Field(new hydra.core.Name("remainder"), (remainder).value)))));
  }

  static hydra.phantoms.TTerm<String> parseErrorMessage(hydra.phantoms.TTerm<hydra.parsing.ParseError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.parsing.ParseError"), new hydra.core.Name("message"))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> parseErrorRemainder(hydra.phantoms.TTerm<hydra.parsing.ParseError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.parsing.ParseError"), new hydra.core.Name("remainder"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.parsing.ParseError> parseErrorWithMessage(hydra.phantoms.TTerm<hydra.parsing.ParseError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.parsing.ParseError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("message"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("remainder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.parsing.ParseError"), new hydra.core.Name("remainder"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.parsing.ParseError> parseErrorWithRemainder(hydra.phantoms.TTerm<hydra.parsing.ParseError> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.parsing.ParseError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.parsing.ParseError"), new hydra.core.Name("message"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("remainder"), (newVal).value)))));
  }

  static <A> hydra.phantoms.TTerm<hydra.parsing.ParseResult<A>> parseResultFailure(hydra.phantoms.TTerm<hydra.parsing.ParseError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.parsing.ParseResult"), new hydra.core.Field(new hydra.core.Name("failure"), (x).value))));
  }

  static <A> hydra.phantoms.TTerm<hydra.parsing.ParseResult<A>> parseResultSuccess(hydra.phantoms.TTerm<hydra.parsing.ParseSuccess<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.parsing.ParseResult"), new hydra.core.Field(new hydra.core.Name("success"), (x).value))));
  }

  static <A> hydra.phantoms.TTerm<hydra.parsing.ParseSuccess<A>> parseSuccess(hydra.phantoms.TTerm<A> value, hydra.phantoms.TTerm<String> remainder) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.parsing.ParseSuccess"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("value"), (value).value),
      new hydra.core.Field(new hydra.core.Name("remainder"), (remainder).value)))));
  }

  static <A> hydra.phantoms.TTerm<String> parseSuccessRemainder(hydra.phantoms.TTerm<hydra.parsing.ParseSuccess<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.parsing.ParseSuccess"), new hydra.core.Name("remainder"))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<A> parseSuccessValue(hydra.phantoms.TTerm<hydra.parsing.ParseSuccess<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.parsing.ParseSuccess"), new hydra.core.Name("value"))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<hydra.parsing.ParseSuccess<A>> parseSuccessWithRemainder(hydra.phantoms.TTerm<hydra.parsing.ParseSuccess<A>> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.parsing.ParseSuccess"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.parsing.ParseSuccess"), new hydra.core.Name("value"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("remainder"), (newVal).value)))));
  }

  static <A> hydra.phantoms.TTerm<hydra.parsing.ParseSuccess<A>> parseSuccessWithValue(hydra.phantoms.TTerm<hydra.parsing.ParseSuccess<A>> original, hydra.phantoms.TTerm<A> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.parsing.ParseSuccess"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("value"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("remainder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.parsing.ParseSuccess"), new hydra.core.Name("remainder"))), (original).value)))))));
  }

  static <A> hydra.phantoms.TTerm<hydra.parsing.Parser<A>> parser(hydra.phantoms.TTerm<java.util.function.Function<String, hydra.parsing.ParseResult<A>>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.parsing.Parser"), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<java.util.function.Function<String, hydra.parsing.ParseResult<A>>> unParser(hydra.phantoms.TTerm<hydra.parsing.Parser<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.parsing.Parser")), (x).value)));
  }
}
