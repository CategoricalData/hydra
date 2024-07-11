// Note: this is an automatically generated file. Do not edit.

package hydra.tier2;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.Type;
import hydra.util.Opt;

/**
 * A module for miscellaneous tier-2 functions and constants.
 */
public interface Tier2 {
  static <S> hydra.compute.Flow<S, S> getState() {
    return new hydra.compute.Flow((java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState>>) (s0 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState>) (t0 -> {
      Flow<Void, S> f1 = Flows.pure(null);
      FlowState<Void, S> fs1 = f1.value.apply(s0).apply(t0);
      return ((((fs1)).value).map((ignored -> new hydra.compute.FlowState(Opt.of(((fs1)).state), ((fs1)).state, ((fs1)).trace)))).orElse(new hydra.compute.FlowState(Opt.empty(), ((fs1)).state, ((fs1)).trace));
    })));
  }

  static <S> hydra.compute.Flow<S, java.lang.Void> putState(S cx) {
    return new hydra.compute.Flow<S, Void>((s0 -> (t0 -> {
      Flow<S, Void> ff = Flows.pure(null);
      hydra.compute.FlowState<S, Void> f1 = ((ff.value).apply((s0))).apply((t0));
      return new hydra.compute.FlowState<>(f1.value, cx, f1.trace);
    })));
  }

  static <A> hydra.compute.Flow<hydra.graph.Graph<A>, hydra.core.Type<A>> requireTypeAnnotation(hydra.core.Term<A> term) {
    java.util.function.Function<Opt<Type<A>>, hydra.compute.Flow<hydra.graph.Graph<A>, hydra.core.Type<A>>> checkType = (java.util.function.Function<Opt<Type<A>>, hydra.compute.Flow<hydra.graph.Graph<A>, hydra.core.Type<A>>>) (v1 -> (((v1)).map((java.util.function.Function<hydra.core.Type<A>, hydra.compute.Flow<hydra.graph.Graph<A>, hydra.core.Type<A>>>) (s0 -> hydra.lib.flows.Pure.apply((s0))))).orElse(hydra.lib.flows.Fail.apply("missing type annotation")));
    java.util.function.Function<hydra.graph.AnnotationClass<A>, hydra.compute.Flow<hydra.graph.Graph<A>, hydra.core.Type<A>>> annsToType = (java.util.function.Function<hydra.graph.AnnotationClass<A>, hydra.compute.Flow<hydra.graph.Graph<A>, hydra.core.Type<A>>>) (anns -> hydra.lib.flows.Bind.apply(
      (((anns)).termType).apply((term)),
      (checkType)));
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.Map.apply(
        (java.util.function.Function<hydra.graph.Graph<A>, hydra.graph.AnnotationClass<A>>) (s1 -> ((s1)).annotations),
        (hydra.tier2.Tier2.getState())),
      (annsToType));
  }
  
  static <S, Y> java.util.function.Function<String, hydra.compute.Flow<S, Y>> unexpected(String cat) {
    return (java.util.function.Function<String, hydra.compute.Flow<S, Y>>) (obj -> hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "expected ",
          (cat))),
        " but found: ")),
      (obj)))));
  }
}
