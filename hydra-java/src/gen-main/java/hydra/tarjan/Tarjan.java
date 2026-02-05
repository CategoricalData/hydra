// Note: this is an automatically generated file. Do not edit.

package hydra.tarjan;

/**
 * This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC.
 */
public interface Tarjan {
  static <T0> hydra.util.Tuple.Tuple2<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>> adjacencyListsToGraph(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>> edges0) {
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>>>> indexedEdges = new hydra.util.Lazy<>(() -> hydra.tarjan.Tarjan.<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>>adjacencyListsToGraph_indexedEdges(hydra.tarjan.Tarjan.<T0, java.util.List<T0>>adjacencyListsToGraph_sortedEdges((edges0))));
    hydra.util.Lazy<java.util.Map<Integer, java.util.List<Integer>>> graph = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>>, hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>) (vkNeighbors -> {
        hydra.util.Lazy<Integer> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((vkNeighbors)));
        return (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(v.get(), hydra.lib.maybes.MapMaybe.apply(
          (java.util.function.Function<T0, hydra.util.Maybe<Integer>>) (k -> hydra.lib.maps.Lookup.apply(
            (k),
            hydra.tarjan.Tarjan.<Integer, T0, java.util.List<T0>>adjacencyListsToGraph_keyToVertex(indexedEdges.get()))),
          hydra.tarjan.Tarjan.<T0, java.util.List<T0>>adjacencyListsToGraph_neighbors(hydra.tarjan.Tarjan.<Integer, hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>>adjacencyListsToGraph_kNeighbors((vkNeighbors)))))));
      }),
      indexedEdges.get())));
    return (hydra.util.Tuple.Tuple2<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>>) ((hydra.util.Tuple.Tuple2<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>>) (new hydra.util.Tuple.Tuple2<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>>(graph.get(), (java.util.function.Function<Integer, T0>) (v1 -> hydra.tarjan.Tarjan.<Integer, T0>adjacencyListsToGraph_vertexToKey(
      hydra.tarjan.Tarjan.<Integer, T0, java.util.List<T0>>adjacencyListsToGraph_vertexMap(indexedEdges.get()),
      (v1))))));
  }
  
  static <T0, T1> java.util.List<hydra.util.Tuple.Tuple2<T0, T1>> adjacencyListsToGraph_sortedEdges(java.util.List<hydra.util.Tuple.Tuple2<T0, T1>> edges0) {
    return hydra.lib.lists.SortOn.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, T0>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, T0>) ((hydra.lib.pairs.First::apply))),
      (edges0));
  }
  
  static <T0> java.util.List<hydra.util.Tuple.Tuple2<Integer, T0>> adjacencyListsToGraph_indexedEdges(java.util.List<T0> sortedEdges) {
    return hydra.lib.lists.Zip.apply(
      hydra.lib.math.Range.apply(
        0,
        hydra.lib.lists.Length.apply((sortedEdges))),
      (sortedEdges));
  }
  
  static <T0, T1, T2> java.util.Map<T1, T0> adjacencyListsToGraph_keyToVertex(java.util.List<hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T1, T2>>> indexedEdges) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T1, T2>>, hydra.util.Tuple.Tuple2<T1, T0>>) (vkNeighbors -> {
        hydra.util.Lazy<T0> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((vkNeighbors)));
        return (hydra.util.Tuple.Tuple2<T1, T0>) ((hydra.util.Tuple.Tuple2<T1, T0>) (new hydra.util.Tuple.Tuple2<T1, T0>(hydra.tarjan.Tarjan.<T1, T2>adjacencyListsToGraph_k2(hydra.tarjan.Tarjan.<T0, hydra.util.Tuple.Tuple2<T1, T2>>adjacencyListsToGraph_kNeighbors3((vkNeighbors))), v.get())));
      }),
      (indexedEdges)));
  }
  
  static <T0, T1, T2> java.util.Map<T0, T1> adjacencyListsToGraph_vertexMap(java.util.List<hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T1, T2>>> indexedEdges) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T1, T2>>, hydra.util.Tuple.Tuple2<T0, T1>>) (vkNeighbors -> {
        hydra.util.Lazy<T0> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((vkNeighbors)));
        return (hydra.util.Tuple.Tuple2<T0, T1>) ((hydra.util.Tuple.Tuple2<T0, T1>) (new hydra.util.Tuple.Tuple2<T0, T1>(v.get(), hydra.tarjan.Tarjan.<T1, T2>adjacencyListsToGraph_k(hydra.tarjan.Tarjan.<T0, hydra.util.Tuple.Tuple2<T1, T2>>adjacencyListsToGraph_kNeighbors2((vkNeighbors))))));
      }),
      (indexedEdges)));
  }
  
  static <T0, T1> T1 adjacencyListsToGraph_vertexToKey(java.util.Map<T0, T1> vertexMap, T0 v) {
    return hydra.lib.maybes.FromJust.apply(hydra.lib.maps.Lookup.apply(
      (v),
      (vertexMap)));
  }
  
  static <T0, T1> T1 adjacencyListsToGraph_kNeighbors(hydra.util.Tuple.Tuple2<T0, T1> vkNeighbors) {
    return hydra.lib.pairs.Second.apply((vkNeighbors));
  }
  
  static <T0, T1> T1 adjacencyListsToGraph_neighbors(hydra.util.Tuple.Tuple2<T0, T1> kNeighbors) {
    return hydra.lib.pairs.Second.apply((kNeighbors));
  }
  
  static <T0, T1> T1 adjacencyListsToGraph_kNeighbors2(hydra.util.Tuple.Tuple2<T0, T1> vkNeighbors) {
    return hydra.lib.pairs.Second.apply((vkNeighbors));
  }
  
  static <T0, T1> T0 adjacencyListsToGraph_k(hydra.util.Tuple.Tuple2<T0, T1> kNeighbors) {
    return hydra.lib.pairs.First.apply((kNeighbors));
  }
  
  static <T0, T1> T1 adjacencyListsToGraph_kNeighbors3(hydra.util.Tuple.Tuple2<T0, T1> vkNeighbors) {
    return hydra.lib.pairs.Second.apply((vkNeighbors));
  }
  
  static <T0, T1> T0 adjacencyListsToGraph_k2(hydra.util.Tuple.Tuple2<T0, T1> kNeighbors) {
    return hydra.lib.pairs.First.apply((kNeighbors));
  }
  
  static java.util.List<java.util.List<Integer>> stronglyConnectedComponents(java.util.Map<Integer, java.util.List<Integer>> graph) {
    java.util.function.Function<Integer, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>> processVertex = (java.util.function.Function<Integer, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (v -> hydra.lib.flows.Bind.apply(
      hydra.lib.flows.Map.apply(
        (java.util.function.Function<hydra.topology.TarjanState, Boolean>) (st -> hydra.lib.maps.Member.apply(
          (v),
          ((st)).indices)),
        hydra.monads.Monads.<hydra.topology.TarjanState>getState()),
      (java.util.function.Function<Boolean, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (visited -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.Not.apply((visited)),
        () -> hydra.tarjan.Tarjan.strongConnect(
          (graph),
          (v)),
        () -> hydra.lib.flows.Pure.apply(null)))));
    hydra.util.Lazy<java.util.List<Integer>> verts = new hydra.util.Lazy<>(() -> hydra.lib.maps.Keys.apply((graph)));
    hydra.util.Lazy<hydra.topology.TarjanState> finalState = new hydra.util.Lazy<>(() -> hydra.monads.Monads.exec(
      hydra.lib.flows.MapList.apply(
        (processVertex),
        verts.get()),
      hydra.tarjan.Tarjan.initialState()));
    return hydra.lib.lists.Reverse.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>) ((hydra.lib.lists.Sort::apply)),
      (finalState.get()).sccs));
  }
  
  static hydra.topology.TarjanState initialState() {
    return new hydra.topology.TarjanState(0, (java.util.Map<Integer, Integer>) ((java.util.Map<Integer, Integer>) (hydra.lib.maps.Empty.<Integer, Integer>apply())), (java.util.Map<Integer, Integer>) ((java.util.Map<Integer, Integer>) (hydra.lib.maps.Empty.<Integer, Integer>apply())), (java.util.List<Integer>) (java.util.List.<Integer>of()), (java.util.Set<Integer>) (hydra.lib.sets.Empty.<Integer>apply()), (java.util.List<java.util.List<Integer>>) (java.util.List.<java.util.List<Integer>>of()));
  }
  
  static hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>> popStackUntil(Integer v) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<Integer>, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.List<Integer>, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>>) (acc -> {
      java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>> succeed = (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>>) (st -> {
        hydra.util.Lazy<Integer> x = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(((st)).stack));
        hydra.util.Lazy<java.util.List<Integer>> acc_ = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
          x.get(),
          (acc)));
        hydra.util.Lazy<java.util.List<Integer>> xs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(((st)).stack));
        hydra.topology.TarjanState newSt = new hydra.topology.TarjanState(((st)).counter, ((st)).indices, ((st)).lowLinks, xs.get(), ((st)).onStack, ((st)).sccs);
        hydra.util.Lazy<hydra.topology.TarjanState> newSt2 = new hydra.util.Lazy<>(() -> new hydra.topology.TarjanState(((newSt)).counter, ((newSt)).indices, ((newSt)).lowLinks, ((newSt)).stack, hydra.lib.sets.Delete.apply(
          x.get(),
          ((st)).onStack), ((newSt)).sccs));
        return hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.putState(newSt2.get()),
          (java.util.function.Function<java.lang.Void, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>>) (ignored -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              x.get(),
              (v)),
            () -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Reverse.apply(acc_.get())),
            () -> (go.get()).apply(acc_.get()))));
      });
      return hydra.lib.flows.Bind.apply(
        hydra.monads.Monads.<hydra.topology.TarjanState>getState(),
        (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>>) (st -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(((st)).stack),
          () -> hydra.lib.flows.Fail.apply("popStackUntil: empty stack"),
          () -> ((succeed)).apply((st)))));
    }));
    return (go.get()).apply((java.util.List<Integer>) (java.util.List.<Integer>of()));
  }
  
  static hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void> strongConnect(java.util.Map<Integer, java.util.List<Integer>> graph, Integer v) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.topology.TarjanState>getState(),
      (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (st -> {
        Integer i = ((st)).counter;
        hydra.util.Lazy<java.util.List<Integer>> neighbors = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.apply(
          (java.util.List<Integer>) (java.util.List.<Integer>of()),
          (v),
          (graph)));
        hydra.util.Lazy<hydra.topology.TarjanState> newSt = new hydra.util.Lazy<>(() -> new hydra.topology.TarjanState(hydra.lib.math.Add.apply(
          (i),
          1), hydra.lib.maps.Insert.apply(
          (v),
          (i),
          ((st)).indices), hydra.lib.maps.Insert.apply(
          (v),
          (i),
          ((st)).lowLinks), hydra.lib.lists.Cons.apply(
          (v),
          ((st)).stack), hydra.lib.sets.Insert.apply(
          (v),
          ((st)).onStack), ((st)).sccs));
        java.util.function.Function<Integer, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>> processNeighbor = (java.util.function.Function<Integer, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (w -> {
          java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>> lowLink = (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (st_ -> {
            hydra.util.Lazy<Integer> idx_w = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.apply(
              hydra.constants.Constants.maxInt32(),
              (w),
              ((st_)).indices));
            hydra.util.Lazy<Integer> low_v = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.apply(
              hydra.constants.Constants.maxInt32(),
              (v),
              ((st_)).lowLinks));
            return hydra.lib.flows.Bind.apply(
              hydra.monads.Monads.modify((java.util.function.Function<hydra.topology.TarjanState, hydra.topology.TarjanState>) (s -> new hydra.topology.TarjanState(((s)).counter, ((s)).indices, hydra.lib.maps.Insert.apply(
                (v),
                hydra.lib.equality.Min.apply(
                  low_v.get(),
                  idx_w.get()),
                ((s)).lowLinks), ((s)).stack, ((s)).onStack, ((s)).sccs))),
              (java.util.function.Function<java.lang.Void, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (ignored -> hydra.lib.flows.Pure.apply(null)));
          });
          return hydra.lib.flows.Bind.apply(
            hydra.monads.Monads.<hydra.topology.TarjanState>getState(),
            (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (st_ -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
                (w),
                ((st_)).indices)),
              () -> hydra.lib.flows.Bind.apply(
                hydra.tarjan.Tarjan.strongConnect(
                  (graph),
                  (w)),
                (java.util.function.Function<java.lang.Void, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (ignored -> hydra.lib.flows.Bind.apply(
                  hydra.monads.Monads.<hydra.topology.TarjanState>getState(),
                  (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (stAfter -> {
                    hydra.util.Lazy<Integer> low_v = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.apply(
                      hydra.constants.Constants.maxInt32(),
                      (v),
                      ((stAfter)).lowLinks));
                    hydra.util.Lazy<Integer> low_w = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.apply(
                      hydra.constants.Constants.maxInt32(),
                      (w),
                      ((stAfter)).lowLinks));
                    return hydra.lib.flows.Bind.apply(
                      hydra.monads.Monads.modify((java.util.function.Function<hydra.topology.TarjanState, hydra.topology.TarjanState>) (s -> new hydra.topology.TarjanState(((s)).counter, ((s)).indices, hydra.lib.maps.Insert.apply(
                        (v),
                        hydra.lib.equality.Min.apply(
                          low_v.get(),
                          low_w.get()),
                        ((s)).lowLinks), ((s)).stack, ((s)).onStack, ((s)).sccs))),
                      (java.util.function.Function<java.lang.Void, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (_2 -> hydra.lib.flows.Pure.apply(null)));
                  })))),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.sets.Member.apply(
                  (w),
                  ((st_)).onStack),
                () -> ((lowLink)).apply((st_)),
                () -> hydra.lib.flows.Pure.apply(null)))));
        });
        return hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.putState(newSt.get()),
          (java.util.function.Function<java.lang.Void, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (ignored -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.MapList.apply(
              (processNeighbor),
              neighbors.get()),
            (java.util.function.Function<java.util.List<java.lang.Void>, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (_2 -> hydra.lib.flows.Bind.apply(
              hydra.monads.Monads.<hydra.topology.TarjanState>getState(),
              (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (stFinal -> {
                hydra.util.Lazy<Integer> idx_v = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.apply(
                  hydra.constants.Constants.maxInt32(),
                  (v),
                  ((stFinal)).indices));
                hydra.util.Lazy<Integer> low_v = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.apply(
                  hydra.constants.Constants.maxInt32(),
                  (v),
                  ((stFinal)).lowLinks));
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    low_v.get(),
                    idx_v.get()),
                  () -> hydra.lib.flows.Bind.apply(
                    hydra.tarjan.Tarjan.popStackUntil((v)),
                    (java.util.function.Function<java.util.List<Integer>, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (comp -> hydra.lib.flows.Bind.apply(
                      hydra.monads.Monads.modify((java.util.function.Function<hydra.topology.TarjanState, hydra.topology.TarjanState>) (s -> new hydra.topology.TarjanState(((s)).counter, ((s)).indices, ((s)).lowLinks, ((s)).stack, ((s)).onStack, hydra.lib.lists.Cons.apply(
                        (comp),
                        ((s)).sccs)))),
                      (java.util.function.Function<java.lang.Void, hydra.compute.Flow<hydra.topology.TarjanState, java.lang.Void>>) (_3 -> hydra.lib.flows.Pure.apply(null))))),
                  () -> hydra.lib.flows.Pure.apply(null));
              }))))));
      }));
  }
}
