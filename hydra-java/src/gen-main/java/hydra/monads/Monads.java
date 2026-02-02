// Note: this is an automatically generated file. Do not edit.

package hydra.monads;

/**
 * Functions for working with Hydra's 'flow' and other monads.
 */
public interface Monads {
  static <T0, T1, T2> hydra.compute.Flow<T0, T2> bind(hydra.compute.Flow<T0, T1> l, java.util.function.Function<T1, hydra.compute.Flow<T0, T2>> r) {
    return (hydra.compute.Flow<T0, T2>) ((hydra.compute.Flow<T0, T2>) (new hydra.compute.Flow((java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T2>>>) (v1 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T2>>) (v2 -> hydra.monads.Monads.<T0, T1, T2>bind_q(
      (l),
      (r),
      (v1),
      (v2)))))));
  }
  
  static <T0, T1, T2> hydra.compute.FlowState<T0, T2> bind_q(hydra.compute.Flow<T0, T1> l, java.util.function.Function<T1, hydra.compute.Flow<T0, T2>> r, T0 s0, hydra.compute.Trace t0) {
    return hydra.lib.maybes.Maybe.apply(
      (hydra.compute.FlowState<T0, T2>) ((hydra.compute.FlowState<T0, T2>) (new hydra.compute.FlowState<T0, T2>((hydra.util.Maybe<T2>) (hydra.util.Maybe.<T2>nothing()), ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) (projected -> projected.state))).apply(hydra.monads.Monads.<T0, T1>bind_fs1(
        (l),
        (s0),
        (t0))), ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) (projected -> projected.trace))).apply(hydra.monads.Monads.<T0, T1>bind_fs1(
        (l),
        (s0),
        (t0)))))),
      (java.util.function.Function<T1, hydra.compute.FlowState<T0, T2>>) (v -> ((((java.util.function.Function<hydra.compute.Flow<T0, T2>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T2>>>>) ((java.util.function.Function<hydra.compute.Flow<T0, T2>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T2>>>>) (wrapped -> ((wrapped)).value))).apply(((r)).apply((v)))).apply(((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) (projected -> projected.state))).apply(hydra.monads.Monads.<T0, T1>bind_fs1(
        (l),
        (s0),
        (t0))))).apply(((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) (projected -> projected.trace))).apply(hydra.monads.Monads.<T0, T1>bind_fs1(
        (l),
        (s0),
        (t0))))),
      ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) (projected -> projected.value))).apply(hydra.monads.Monads.<T0, T1>bind_fs1(
        (l),
        (s0),
        (t0))));
  }
  
  static <T0, T1> hydra.compute.FlowState<T0, T1> bind_fs1(hydra.compute.Flow<T0, T1> l, T0 s0, hydra.compute.Trace t0) {
    return ((((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) ((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) (wrapped -> ((wrapped)).value))).apply((l))).apply((s0))).apply((t0));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, T1> eitherToFlow(java.util.function.Function<T0, String> formatError, hydra.util.Either<T0, T1> e) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<T0, hydra.compute.Flow<T2, T1>>) (l -> hydra.monads.Monads.<T2, T1>fail(((formatError)).apply((l)))),
      (java.util.function.Function<T1, hydra.compute.Flow<T2, T1>>) (r -> hydra.monads.Monads.<T1, T2>pure((r))),
      (e));
  }
  
  hydra.compute.Trace emptyTrace = new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())));
  
  static <T0, T1> T0 exec(hydra.compute.Flow<T0, T1> f, T0 s0) {
    return ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) (projected -> projected.state))).apply(((((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) ((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) (wrapped -> ((wrapped)).value))).apply((f))).apply((s0))).apply((hydra.monads.Monads.emptyTrace)));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> fail(String msg) {
    return (hydra.compute.Flow<T0, T1>) ((hydra.compute.Flow<T0, T1>) (new hydra.compute.Flow((java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>) (s -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>) (t -> (hydra.compute.FlowState<T0, T1>) ((hydra.compute.FlowState<T0, T1>) (new hydra.compute.FlowState<T0, T1>((hydra.util.Maybe<T1>) (hydra.util.Maybe.<T1>nothing()), (s), hydra.monads.Monads.pushError(
      (msg),
      (t))))))))));
  }
  
  static <T0, T1> Boolean flowSucceeds(T0 s, hydra.compute.Flow<T0, T1> f) {
    return hydra.lib.maybes.IsJust.apply(((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) (projected -> projected.value))).apply(((((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) ((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) (wrapped -> ((wrapped)).value))).apply((f))).apply((s))).apply((hydra.monads.Monads.emptyTrace))));
  }
  
  static <T0, T1> T0 fromFlow(T0 def, T1 cx, hydra.compute.Flow<T1, T0> f) {
    return hydra.lib.maybes.Maybe.apply(
      (def),
      (java.util.function.Function<T0, T0>) (xmo -> (xmo)),
      ((java.util.function.Function<hydra.compute.FlowState<T1, T0>, hydra.util.Maybe<T0>>) ((java.util.function.Function<hydra.compute.FlowState<T1, T0>, hydra.util.Maybe<T0>>) (projected -> projected.value))).apply(((((java.util.function.Function<hydra.compute.Flow<T1, T0>, java.util.function.Function<T1, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T1, T0>>>>) ((java.util.function.Function<hydra.compute.Flow<T1, T0>, java.util.function.Function<T1, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T1, T0>>>>) (wrapped -> ((wrapped)).value))).apply((f))).apply((cx))).apply((hydra.monads.Monads.emptyTrace))));
  }
  
  static <T0> hydra.compute.Flow<T0, T0> getState() {
    return (hydra.compute.Flow<T0, T0>) ((hydra.compute.Flow<T0, T0>) (new hydra.compute.Flow((java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T0>>>) (s0 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T0>>) (t0 -> {
      hydra.compute.Trace t = ((java.util.function.Function<hydra.compute.FlowState<T0, Boolean>, hydra.compute.Trace>) ((java.util.function.Function<hydra.compute.FlowState<T0, Boolean>, hydra.compute.Trace>) (projected -> projected.trace))).apply(hydra.monads.Monads.<T0>getState_fs1(
        (s0),
        (t0)));
      hydra.util.Maybe<Boolean> v = ((java.util.function.Function<hydra.compute.FlowState<T0, Boolean>, hydra.util.Maybe<Boolean>>) ((java.util.function.Function<hydra.compute.FlowState<T0, Boolean>, hydra.util.Maybe<Boolean>>) (projected -> projected.value))).apply(hydra.monads.Monads.<T0>getState_fs1(
        (s0),
        (t0)));
      return hydra.lib.maybes.Maybe.apply(
        (hydra.compute.FlowState<T0, T0>) ((hydra.compute.FlowState<T0, T0>) (new hydra.compute.FlowState<T0, T0>((hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()), hydra.monads.Monads.<T0, Boolean>getState_s(hydra.monads.Monads.<T0>getState_fs1(
          (s0),
          (t0))), (t)))),
        (java.util.function.Function<Boolean, hydra.compute.FlowState<T0, T0>>) (ignored -> (hydra.compute.FlowState<T0, T0>) ((hydra.compute.FlowState<T0, T0>) (new hydra.compute.FlowState<T0, T0>(hydra.util.Maybe.just(hydra.monads.Monads.<T0, Boolean>getState_s(hydra.monads.Monads.<T0>getState_fs1(
          (s0),
          (t0)))), hydra.monads.Monads.<T0, Boolean>getState_s(hydra.monads.Monads.<T0>getState_fs1(
          (s0),
          (t0))), (t))))),
        (v));
    })))));
  }
  
  static <T0> hydra.compute.FlowState<T0, Boolean> getState_fs1(T0 s0, hydra.compute.Trace t0) {
    return ((((java.util.function.Function<hydra.compute.Flow<T0, Boolean>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, Boolean>>>>) ((java.util.function.Function<hydra.compute.Flow<T0, Boolean>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, Boolean>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.<Boolean, T0>pure(true))).apply((s0))).apply((t0));
  }
  
  static <T0, T1> T0 getState_s(hydra.compute.FlowState<T0, T1> fs1) {
    return ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) (projected -> projected.state))).apply((fs1));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, T1> map(java.util.function.Function<T0, T1> f, hydra.compute.Flow<T2, T0> f1) {
    return (hydra.compute.Flow<T2, T1>) ((hydra.compute.Flow<T2, T1>) (new hydra.compute.Flow((java.util.function.Function<T2, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T2, T1>>>) (s0 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T2, T1>>) (t0 -> (hydra.compute.FlowState<T2, T1>) ((hydra.compute.FlowState<T2, T1>) (new hydra.compute.FlowState<T2, T1>(hydra.lib.maybes.Map.apply(
      (f),
      ((java.util.function.Function<hydra.compute.FlowState<T2, T0>, hydra.util.Maybe<T0>>) ((java.util.function.Function<hydra.compute.FlowState<T2, T0>, hydra.util.Maybe<T0>>) (projected -> projected.value))).apply(hydra.monads.Monads.<T2, T0>map_f2(
        (f1),
        (s0),
        (t0)))), ((java.util.function.Function<hydra.compute.FlowState<T2, T0>, T2>) ((java.util.function.Function<hydra.compute.FlowState<T2, T0>, T2>) (projected -> projected.state))).apply(hydra.monads.Monads.<T2, T0>map_f2(
      (f1),
      (s0),
      (t0))), ((java.util.function.Function<hydra.compute.FlowState<T2, T0>, hydra.compute.Trace>) ((java.util.function.Function<hydra.compute.FlowState<T2, T0>, hydra.compute.Trace>) (projected -> projected.trace))).apply(hydra.monads.Monads.<T2, T0>map_f2(
      (f1),
      (s0),
      (t0)))))))))));
  }
  
  static <T0, T1> hydra.compute.FlowState<T0, T1> map_f2(hydra.compute.Flow<T0, T1> f1, T0 s0, hydra.compute.Trace t0) {
    return ((((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) ((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) (wrapped -> ((wrapped)).value))).apply((f1))).apply((s0))).apply((t0));
  }
  
  static <T0> hydra.compute.Flow<T0, Boolean> modify(java.util.function.Function<T0, T0> f) {
    return hydra.monads.Monads.<T0, T0, Boolean>bind(
      hydra.monads.Monads.<T0>getState(),
      (java.util.function.Function<T0, hydra.compute.Flow<T0, Boolean>>) (s -> hydra.monads.Monads.<T0>putState(((f)).apply((s)))));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> mutateTrace(java.util.function.Function<hydra.compute.Trace, hydra.util.Either<String, hydra.compute.Trace>> mutate, java.util.function.Function<hydra.compute.Trace, java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace>> restore, hydra.compute.Flow<T0, T1> f) {
    return (hydra.compute.Flow<T0, T1>) ((hydra.compute.Flow<T0, T1>) (new hydra.compute.Flow((java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>) (v1 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>) (v2 -> hydra.monads.Monads.mutateTrace_flowFun(
      (f),
      (java.util.function.Function<String, java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace>>) (p0 -> p1 -> hydra.monads.Monads.pushError(
        (p0),
        (p1))),
      (mutate),
      (restore),
      (v1),
      (v2)))))));
  }
  
  static <T0, T1, T2> T1 mutateTrace_choose(java.util.function.Function<T0, T1> forLeft, java.util.function.Function<T2, T1> forRight, hydra.util.Either<T0, T2> e) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<T0, T1>) (l -> ((forLeft)).apply((l))),
      (java.util.function.Function<T2, T1>) (r -> ((forRight)).apply((r))),
      (e));
  }
  
  static <T0, T1, T2, T3> hydra.compute.FlowState<T0, T1> mutateTrace_flowFun(hydra.compute.Flow<T0, T1> f, java.util.function.Function<T2, java.util.function.Function<T3, hydra.compute.Trace>> hydra_monads_pushError2, java.util.function.Function<T3, hydra.util.Either<T2, hydra.compute.Trace>> mutate, java.util.function.Function<T3, java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace>> restore, T0 s0, T3 t0) {
    return hydra.monads.Monads.mutateTrace_choose(
      (java.util.function.Function<T2, hydra.compute.FlowState<T0, T1>>) (v1 -> hydra.monads.Monads.<T2, T3, T0, T1>mutateTrace_forLeft(
        (hydra_monads_pushError2),
        (s0),
        (t0),
        (v1))),
      (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>) (v1 -> hydra.monads.Monads.<T0, T1, T3>mutateTrace_forRight(
        (f),
        (restore),
        (s0),
        (t0),
        (v1))),
      ((mutate)).apply((t0)));
  }
  
  static <T0, T1, T2, T3> hydra.compute.FlowState<T2, T3> mutateTrace_forLeft(java.util.function.Function<T0, java.util.function.Function<T1, hydra.compute.Trace>> hydra_monads_pushError2, T2 s0, T1 t0, T0 msg) {
    return (hydra.compute.FlowState<T2, T3>) ((hydra.compute.FlowState<T2, T3>) (new hydra.compute.FlowState<T2, T3>((hydra.util.Maybe<T3>) (hydra.util.Maybe.<T3>nothing()), (s0), (((hydra_monads_pushError2)).apply((msg))).apply((t0)))));
  }
  
  static <T0, T1, T2> hydra.compute.FlowState<T0, T1> mutateTrace_forRight(hydra.compute.Flow<T0, T1> f, java.util.function.Function<T2, java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace>> restore, T0 s0, T2 t0, hydra.compute.Trace t1) {
    return (hydra.compute.FlowState<T0, T1>) ((hydra.compute.FlowState<T0, T1>) (new hydra.compute.FlowState<T0, T1>(((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) (projected -> projected.value))).apply(hydra.monads.Monads.<T0, T1>mutateTrace_f2(
      (f),
      (s0),
      (t1))), ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) (projected -> projected.state))).apply(hydra.monads.Monads.<T0, T1>mutateTrace_f2(
      (f),
      (s0),
      (t1))), (((restore)).apply((t0))).apply(((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) (projected -> projected.trace))).apply(hydra.monads.Monads.<T0, T1>mutateTrace_f2(
      (f),
      (s0),
      (t1)))))));
  }
  
  static <T0, T1> hydra.compute.FlowState<T0, T1> mutateTrace_f2(hydra.compute.Flow<T0, T1> f, T0 s0, hydra.compute.Trace t1) {
    return ((((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) ((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) (wrapped -> ((wrapped)).value))).apply((f))).apply((s0))).apply((t1));
  }
  
  static <T0> java.util.List<T0> maybeToList(hydra.util.Maybe<T0> mx) {
    return hydra.lib.maybes.Maybe.apply(
      (java.util.List<T0>) (java.util.List.<T0>of()),
      (java.util.function.Function<T0, java.util.List<T0>>) ((hydra.lib.lists.Pure::apply)),
      (mx));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, T0> pure(T0 xp) {
    return (hydra.compute.Flow<T1, T0>) ((hydra.compute.Flow<T1, T0>) (new hydra.compute.Flow((java.util.function.Function<T1, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T1, T0>>>) (s -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T1, T0>>) (t -> (hydra.compute.FlowState<T1, T0>) ((hydra.compute.FlowState<T1, T0>) (new hydra.compute.FlowState<T1, T0>(hydra.util.Maybe.just((xp)), (s), (t)))))))));
  }
  
  static hydra.compute.Trace pushError(String msg, hydra.compute.Trace t) {
    java.util.function.Function<java.util.List<String>, java.util.List<String>> condenseRepeats = (java.util.function.Function<java.util.List<String>, java.util.List<String>>) (ys -> {
      java.util.function.Function<java.util.List<String>, String> condenseGroup = (java.util.function.Function<java.util.List<String>, String>) (xs -> {
        Integer n = hydra.lib.lists.Length.apply((xs));
        String x = hydra.lib.lists.Head.apply((xs));
        return hydra.lib.logic.IfElse.apply(
          hydra.lib.equality.Equal.apply(
            (n),
            1),
          (x),
          hydra.lib.strings.Cat.apply(java.util.List.of(
            (x),
            " (x",
            hydra.lib.literals.ShowInt32.apply((n)),
            ")")));
      });
      return hydra.lib.lists.Map.apply(
        (condenseGroup),
        hydra.lib.lists.Group.apply((ys)));
    });
    String errorMsg = hydra.lib.strings.Cat.apply(java.util.List.of(
      "Error: ",
      (msg),
      " (",
      hydra.lib.strings.Intercalate.apply(
        " > ",
        ((condenseRepeats)).apply(hydra.lib.lists.Reverse.apply(((t)).stack))),
      ")"));
    return new hydra.compute.Trace(((t)).stack, hydra.lib.lists.Cons.apply(
      (errorMsg),
      ((t)).messages), ((t)).other);
  }
  
  static <T0> hydra.compute.Flow<T0, Boolean> putState(T0 cx) {
    return (hydra.compute.Flow<T0, Boolean>) ((hydra.compute.Flow<T0, Boolean>) (new hydra.compute.Flow((java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, Boolean>>>) (s0 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, Boolean>>) (t0 -> (hydra.compute.FlowState<T0, Boolean>) ((hydra.compute.FlowState<T0, Boolean>) (new hydra.compute.FlowState<T0, Boolean>(((java.util.function.Function<hydra.compute.FlowState<T0, Boolean>, hydra.util.Maybe<Boolean>>) ((java.util.function.Function<hydra.compute.FlowState<T0, Boolean>, hydra.util.Maybe<Boolean>>) (projected -> projected.value))).apply(hydra.monads.Monads.<T0>putState_f1(
      (s0),
      (t0))), (cx), ((java.util.function.Function<hydra.compute.FlowState<T0, Boolean>, hydra.compute.Trace>) ((java.util.function.Function<hydra.compute.FlowState<T0, Boolean>, hydra.compute.Trace>) (projected -> projected.trace))).apply(hydra.monads.Monads.<T0>putState_f1(
      (s0),
      (t0)))))))))));
  }
  
  static <T0> hydra.compute.FlowState<T0, Boolean> putState_f1(T0 s0, hydra.compute.Trace t0) {
    return ((((java.util.function.Function<hydra.compute.Flow<T0, Boolean>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, Boolean>>>>) ((java.util.function.Function<hydra.compute.Flow<T0, Boolean>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, Boolean>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.<Boolean, T0>pure(true))).apply((s0))).apply((t0));
  }
  
  static String traceSummary(hydra.compute.Trace t) {
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>, String> toLine = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>, String>) (pair -> hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "\t",
          (hydra.lib.pairs.First.apply((pair))).value),
        ": "),
      hydra.show.core.Core.term(hydra.lib.pairs.Second.apply((pair)))));
    java.util.List<String> keyvalLines = hydra.lib.logic.IfElse.apply(
      hydra.lib.maps.Null.apply(((t)).other),
      (java.util.List<String>) (java.util.List.<String>of()),
      hydra.lib.lists.Cons.apply(
        "key/value pairs: ",
        hydra.lib.lists.Map.apply(
          (toLine),
          hydra.lib.maps.ToList.apply(((t)).other))));
    java.util.List<String> messageLines = hydra.lib.lists.Nub.apply(((t)).messages);
    return hydra.lib.strings.Intercalate.apply(
      "\n",
      hydra.lib.lists.Concat2.apply(
        (messageLines),
        (keyvalLines)));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> unexpected(String expected, String actual) {
    return hydra.monads.Monads.<T0, T1>fail(hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "expected ",
          (expected)),
        " but found "),
      (actual)));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> warn(String msg, hydra.compute.Flow<T0, T1> b) {
    return (hydra.compute.Flow<T0, T1>) ((hydra.compute.Flow<T0, T1>) (new hydra.compute.Flow((java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>) (s0 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>) (t0 -> {
      java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace> addMessage = (java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace>) (t -> new hydra.compute.Trace(((t)).stack, hydra.lib.lists.Cons.apply(
        hydra.lib.strings.Cat2.apply(
          "Warning: ",
          (msg)),
        ((t)).messages), ((t)).other));
      return (hydra.compute.FlowState<T0, T1>) ((hydra.compute.FlowState<T0, T1>) (new hydra.compute.FlowState<T0, T1>(((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) (projected -> projected.value))).apply(hydra.monads.Monads.<T0, T1>warn_f1(
        (b),
        (s0),
        (t0))), ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, T0>) (projected -> projected.state))).apply(hydra.monads.Monads.<T0, T1>warn_f1(
        (b),
        (s0),
        (t0))), ((addMessage)).apply(((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) (projected -> projected.trace))).apply(hydra.monads.Monads.<T0, T1>warn_f1(
        (b),
        (s0),
        (t0)))))));
    })))));
  }
  
  static <T0, T1> hydra.compute.FlowState<T0, T1> warn_f1(hydra.compute.Flow<T0, T1> b, T0 s0, hydra.compute.Trace t0) {
    return ((((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) ((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) (wrapped -> ((wrapped)).value))).apply((b))).apply((s0))).apply((t0));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> withFlag(hydra.core.Name flag, hydra.compute.Flow<T0, T1> f) {
    java.util.function.Function<hydra.compute.Trace, hydra.util.Either<String, hydra.compute.Trace>> mutate = (java.util.function.Function<hydra.compute.Trace, hydra.util.Either<String, hydra.compute.Trace>>) (t -> hydra.lib.logic.IfElse.apply(
      false,
      (hydra.util.Either<String, hydra.compute.Trace>) ((hydra.util.Either<String, hydra.compute.Trace>) (hydra.util.Either.<String, hydra.compute.Trace>left("never happens"))),
      (hydra.util.Either<String, hydra.compute.Trace>) ((hydra.util.Either<String, hydra.compute.Trace>) (hydra.util.Either.<String, hydra.compute.Trace>right(new hydra.compute.Trace(((t)).stack, ((t)).messages, hydra.lib.maps.Insert.apply(
        (flag),
        new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)),
        ((t)).other)))))));
    return hydra.monads.Monads.<T0, T1>mutateTrace(
      (mutate),
      (java.util.function.Function<hydra.compute.Trace, java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace>>) (v1 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace>) (v2 -> hydra.monads.Monads.withFlag_restore(
        (flag),
        (v1),
        (v2)))),
      (f));
  }
  
  static <T0> hydra.compute.Trace withFlag_restore(hydra.core.Name flag, T0 ignored, hydra.compute.Trace t1) {
    return new hydra.compute.Trace(((t1)).stack, ((t1)).messages, hydra.lib.maps.Delete.apply(
      (flag),
      ((t1)).other));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, T1> withState(T0 cx0, hydra.compute.Flow<T0, T1> f) {
    return (hydra.compute.Flow<T2, T1>) ((hydra.compute.Flow<T2, T1>) (new hydra.compute.Flow((java.util.function.Function<T2, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T2, T1>>>) (cx1 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T2, T1>>) (t1 -> (hydra.compute.FlowState<T2, T1>) ((hydra.compute.FlowState<T2, T1>) (new hydra.compute.FlowState<T2, T1>(((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.util.Maybe<T1>>) (projected -> projected.value))).apply(hydra.monads.Monads.<T0, T1>withState_f1(
      (cx0),
      (f),
      (t1))), (cx1), ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) ((java.util.function.Function<hydra.compute.FlowState<T0, T1>, hydra.compute.Trace>) (projected -> projected.trace))).apply(hydra.monads.Monads.<T0, T1>withState_f1(
      (cx0),
      (f),
      (t1)))))))))));
  }
  
  static <T0, T1> hydra.compute.FlowState<T0, T1> withState_f1(T0 cx0, hydra.compute.Flow<T0, T1> f, hydra.compute.Trace t1) {
    return ((((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) ((java.util.function.Function<hydra.compute.Flow<T0, T1>, java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, T1>>>>) (wrapped -> ((wrapped)).value))).apply((f))).apply((cx0))).apply((t1));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> withTrace(String msg, hydra.compute.Flow<T0, T1> f) {
    java.util.function.Function<hydra.compute.Trace, hydra.util.Either<String, hydra.compute.Trace>> mutate = (java.util.function.Function<hydra.compute.Trace, hydra.util.Either<String, hydra.compute.Trace>>) (t -> hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Gte.apply(
        hydra.lib.lists.Length.apply(((t)).stack),
        (hydra.constants.Constants.maxTraceDepth)),
      (hydra.util.Either<String, hydra.compute.Trace>) ((hydra.util.Either<String, hydra.compute.Trace>) (hydra.util.Either.<String, hydra.compute.Trace>left("maximum trace depth exceeded. This may indicate an infinite loop"))),
      (hydra.util.Either<String, hydra.compute.Trace>) ((hydra.util.Either<String, hydra.compute.Trace>) (hydra.util.Either.<String, hydra.compute.Trace>right(new hydra.compute.Trace(hydra.lib.lists.Cons.apply(
        (msg),
        ((t)).stack), ((t)).messages, ((t)).other))))));
    java.util.function.Function<hydra.compute.Trace, java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace>> restore = (java.util.function.Function<hydra.compute.Trace, java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace>>) (t0 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.Trace>) (t1 -> new hydra.compute.Trace(((t0)).stack, ((t1)).messages, ((t1)).other)));
    return hydra.monads.Monads.<T0, T1>mutateTrace(
      (mutate),
      (restore),
      (f));
  }
}
