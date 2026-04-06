// Note: this is an automatically generated file. Do not edit.

package hydra.encode;

/**
 * Term encoders for hydra.context
 */
public interface Context {
  static hydra.core.Term context(hydra.context.Context x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.context.Context"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("trace"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        (x).trace))),
      new hydra.core.Field(new hydra.core.Name("messages"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        (x).messages))),
      new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.Core::name,
        hydra.encode.Core::term,
        (x).other))))));
  }

  static <T0> hydra.core.Term inContext(java.util.function.Function<T0, hydra.core.Term> e, hydra.context.InContext<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.context.InContext"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("object"), (e).apply(((java.util.function.Function<hydra.context.InContext<T0>, T0>) (projected -> projected.object)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("context"), hydra.encode.Context.context(((java.util.function.Function<hydra.context.InContext<T0>, hydra.context.Context>) (projected -> projected.context)).apply(x))))));
  }
}
