// Note: this is an automatically generated file. Do not edit.

package hydra.encode.compute;

/**
 * Term encoders for hydra.compute
 */
public interface Compute {
  static <T0, T1> hydra.core.Term flowState(java.util.function.Function<T0, hydra.core.Term> s, java.util.function.Function<T1, hydra.core.Term> v, hydra.compute.FlowState<T0, T1> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.compute.FlowState"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        v,
        ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) (projected -> projected.value))).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("state"), (s).apply(((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) (projected -> projected.state))).apply(x))),
      new hydra.core.Field(new hydra.core.Name("trace"), hydra.encode.compute.Compute.trace(((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) (projected -> projected.trace))).apply(x))))));
  }
  
  static hydra.core.Term trace(hydra.compute.Trace x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.compute.Trace"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("stack"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        (x).stack))),
      new hydra.core.Field(new hydra.core.Name("messages"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        (x).messages))),
      new hydra.core.Field(new hydra.core.Name("other"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.core.Core::name,
        hydra.encode.core.Core::term,
        (x).other))))));
  }
}
