// Note: this is an automatically generated file. Do not edit.

package hydra.encode.parsing;

/**
 * Term encoders for hydra.parsing
 */
public interface Parsing {
  static hydra.core.Term parseError(hydra.parsing.ParseError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.parsing.ParseError"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).message))),
      new hydra.core.Field(new hydra.core.Name("remainder"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).remainder))))));
  }
  
  static <T0> hydra.core.Term parseResult(java.util.function.Function<T0, hydra.core.Term> a, hydra.parsing.ParseResult<T0> v1) {
    return ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.core.Term>) ((java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.core.Term>) (u -> (u).accept(new hydra.parsing.ParseResult.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.parsing.ParseResult.Success<T0> y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.parsing.ParseResult"), new hydra.core.Field(new hydra.core.Name("success"), hydra.encode.parsing.Parsing.<T0>parseSuccess(
          a,
          (y).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.parsing.ParseResult.Failure<T0> y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.parsing.ParseResult"), new hydra.core.Field(new hydra.core.Name("failure"), hydra.encode.parsing.Parsing.parseError((y).value))));
      }
    })))).apply(v1);
  }
  
  static <T0> hydra.core.Term parseSuccess(java.util.function.Function<T0, hydra.core.Term> a, hydra.parsing.ParseSuccess<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.parsing.ParseSuccess"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("value"), (a).apply(((java.util.function.Function<hydra.parsing.ParseSuccess<T0>, T0>) (projected -> projected.value)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("remainder"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((java.util.function.Function<hydra.parsing.ParseSuccess<T0>, String>) (projected -> projected.remainder)).apply(x)))))));
  }
}
