// Note: this is an automatically generated file. Do not edit.

package hydra.tarjan;

/**
 * This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC.
 */
public interface Tarjan {
  static <T0> hydra.util.Tuple.Tuple2<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>> adjacencyListsToGraph(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>> edges0) {
    java.util.Map<Integer, java.util.List<Integer>> graph = hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>>, hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>) (vkNeighbors -> {
        Integer v = hydra.lib.pairs.First.apply((vkNeighbors));
        return (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>((v), hydra.lib.maybes.MapMaybe.apply(
          (java.util.function.Function<T0, hydra.util.Maybe<Integer>>) (k -> hydra.lib.maps.Lookup.apply(
            (k),
            hydra.tarjan.Tarjan.<Integer, T0, java.util.List<T0>>adjacencyListsToGraph_keyToVertex(hydra.tarjan.Tarjan.<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>>adjacencyListsToGraph_indexedEdges(hydra.tarjan.Tarjan.<T0, java.util.List<T0>>adjacencyListsToGraph_sortedEdges((edges0)))))),
          hydra.tarjan.Tarjan.<T0, java.util.List<T0>>adjacencyListsToGraph_neighbors(hydra.tarjan.Tarjan.<Integer, hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>>adjacencyListsToGraph_kNeighbors((vkNeighbors)))))));
      }),
      hydra.tarjan.Tarjan.<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>>adjacencyListsToGraph_indexedEdges(hydra.tarjan.Tarjan.<T0, java.util.List<T0>>adjacencyListsToGraph_sortedEdges((edges0)))));
    return (hydra.util.Tuple.Tuple2<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>>) ((hydra.util.Tuple.Tuple2<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>>) (new hydra.util.Tuple.Tuple2<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>>((graph), (java.util.function.Function<Integer, T0>) (v1 -> hydra.tarjan.Tarjan.<Integer, T0>adjacencyListsToGraph_vertexToKey(
      hydra.tarjan.Tarjan.<Integer, T0, java.util.List<T0>>adjacencyListsToGraph_vertexMap(hydra.tarjan.Tarjan.<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>>adjacencyListsToGraph_indexedEdges(hydra.tarjan.Tarjan.<T0, java.util.List<T0>>adjacencyListsToGraph_sortedEdges((edges0)))),
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
        T0 v = hydra.lib.pairs.First.apply((vkNeighbors));
        return (hydra.util.Tuple.Tuple2<T1, T0>) ((hydra.util.Tuple.Tuple2<T1, T0>) (new hydra.util.Tuple.Tuple2<T1, T0>(hydra.tarjan.Tarjan.<T1, T2>adjacencyListsToGraph_k2(hydra.tarjan.Tarjan.<T0, hydra.util.Tuple.Tuple2<T1, T2>>adjacencyListsToGraph_kNeighbors3((vkNeighbors))), (v))));
      }),
      (indexedEdges)));
  }
  
  static <T0, T1, T2> java.util.Map<T0, T1> adjacencyListsToGraph_vertexMap(java.util.List<hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T1, T2>>> indexedEdges) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.util.Tuple.Tuple2<T1, T2>>, hydra.util.Tuple.Tuple2<T0, T1>>) (vkNeighbors -> {
        T0 v = hydra.lib.pairs.First.apply((vkNeighbors));
        return (hydra.util.Tuple.Tuple2<T0, T1>) ((hydra.util.Tuple.Tuple2<T0, T1>) (new hydra.util.Tuple.Tuple2<T0, T1>((v), hydra.tarjan.Tarjan.<T1, T2>adjacencyListsToGraph_k(hydra.tarjan.Tarjan.<T0, hydra.util.Tuple.Tuple2<T1, T2>>adjacencyListsToGraph_kNeighbors2((vkNeighbors))))));
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
    java.util.function.Function<Integer, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>> processVertex = (java.util.function.Function<Integer, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (v -> hydra.lib.flows.Bind.apply(
      hydra.lib.flows.Map.apply(
        (java.util.function.Function<hydra.topology.TarjanState, Boolean>) (st -> hydra.lib.maps.Member.apply(
          (v),
          ((st)).indices)),
        hydra.monads.Monads.<hydra.topology.TarjanState>getState()),
      (java.util.function.Function<Boolean, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (visited -> hydra.lib.logic.IfElse.apply(
        hydra.lib.logic.Not.apply((visited)),
        hydra.tarjan.Tarjan.strongConnect(
          (graph),
          (v)),
        hydra.lib.flows.Pure.apply(true)))));
    java.util.List<Integer> verts = hydra.lib.maps.Keys.apply((graph));
    hydra.topology.TarjanState finalState = hydra.monads.Monads.exec(
      hydra.lib.flows.MapList.apply(
        (processVertex),
        (verts)),
      (hydra.tarjan.Tarjan.initialState));
    return hydra.lib.lists.Reverse.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>) ((hydra.lib.lists.Sort::apply)),
      ((finalState)).sccs));
  }
  
  hydra.topology.TarjanState initialState = new hydra.topology.TarjanState(0, (java.util.Map<Integer, Integer>) ((java.util.Map<Integer, Integer>) (hydra.lib.maps.Empty.<Integer, Integer>apply())), (java.util.Map<Integer, Integer>) ((java.util.Map<Integer, Integer>) (hydra.lib.maps.Empty.<Integer, Integer>apply())), (java.util.List<Integer>) (java.util.List.<Integer>of()), (java.util.Set<Integer>) (hydra.lib.sets.Empty.<Integer>apply()), (java.util.List<java.util.List<Integer>>) (java.util.List.<java.util.List<Integer>>of()));
  
  static hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>> popStackUntil(Integer v) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<Integer>, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.List<Integer>, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>>) (acc -> {
      java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>> succeed = (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>>) (st -> {
        Integer x = hydra.lib.lists.Head.apply(((st)).stack);
        java.util.List<Integer> acc_ = hydra.lib.lists.Cons.apply(
          (x),
          (acc));
        java.util.List<Integer> xs = hydra.lib.lists.Tail.apply(((st)).stack);
        hydra.topology.TarjanState newSt = new hydra.topology.TarjanState(((st)).counter, ((st)).indices, ((st)).lowLinks, (xs), ((st)).onStack, ((st)).sccs);
        hydra.topology.TarjanState newSt2 = new hydra.topology.TarjanState(((newSt)).counter, ((newSt)).indices, ((newSt)).lowLinks, ((newSt)).stack, hydra.lib.sets.Delete.apply(
          (x),
          ((st)).onStack), ((newSt)).sccs);
        return hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.putState((newSt2)),
          (java.util.function.Function<Boolean, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>>) (ignored -> hydra.lib.logic.IfElse.apply(
            hydra.lib.equality.Equal.apply(
              (x),
              (v)),
            hydra.lib.flows.Pure.apply(hydra.lib.lists.Reverse.apply((acc_))),
            (go.get()).apply((acc_)))));
      });
      return hydra.lib.flows.Bind.apply(
        hydra.monads.Monads.<hydra.topology.TarjanState>getState(),
        (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, java.util.List<Integer>>>) (st -> hydra.lib.logic.IfElse.apply(
          hydra.lib.lists.Null.apply(((st)).stack),
          hydra.lib.flows.Fail.apply("popStackUntil: empty stack"),
          ((succeed)).apply((st)))));
    }));
    return (go.get()).apply((java.util.List<Integer>) (java.util.List.<Integer>of()));
  }
  
  static hydra.compute.Flow<hydra.topology.TarjanState, Boolean> strongConnect(java.util.Map<Integer, java.util.List<Integer>> graph, Integer v) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.topology.TarjanState>getState(),
      (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (st -> {
        Integer i = ((st)).counter;
        java.util.List<Integer> neighbors = hydra.lib.maps.FindWithDefault.apply(
          (java.util.List<Integer>) (java.util.List.<Integer>of()),
          (v),
          (graph));
        hydra.topology.TarjanState newSt = new hydra.topology.TarjanState(hydra.lib.math.Add.apply(
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
          ((st)).onStack), ((st)).sccs);
        java.util.function.Function<Integer, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>> processNeighbor = (java.util.function.Function<Integer, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (w -> {
          java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>> lowLink = (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (st_ -> {
            Integer idx_w = hydra.lib.maps.FindWithDefault.apply(
              (hydra.constants.Constants.maxInt32),
              (w),
              ((st_)).indices);
            Integer low_v = hydra.lib.maps.FindWithDefault.apply(
              (hydra.constants.Constants.maxInt32),
              (v),
              ((st_)).lowLinks);
            return hydra.lib.flows.Bind.apply(
              hydra.monads.Monads.modify((java.util.function.Function<hydra.topology.TarjanState, hydra.topology.TarjanState>) (s -> new hydra.topology.TarjanState(((s)).counter, ((s)).indices, hydra.lib.maps.Insert.apply(
                (v),
                hydra.lib.equality.Min.apply(
                  (low_v),
                  (idx_w)),
                ((s)).lowLinks), ((s)).stack, ((s)).onStack, ((s)).sccs))),
              (java.util.function.Function<Boolean, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (ignored -> hydra.lib.flows.Pure.apply(true)));
          });
          return hydra.lib.flows.Bind.apply(
            hydra.monads.Monads.<hydra.topology.TarjanState>getState(),
            (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (st_ -> hydra.lib.logic.IfElse.apply(
              hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
                (w),
                ((st_)).indices)),
              hydra.lib.flows.Bind.apply(
                hydra.tarjan.Tarjan.strongConnect(
                  (graph),
                  (w)),
                (java.util.function.Function<Boolean, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (ignored -> hydra.lib.flows.Bind.apply(
                  hydra.monads.Monads.<hydra.topology.TarjanState>getState(),
                  (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (stAfter -> {
                    Integer low_v = hydra.lib.maps.FindWithDefault.apply(
                      (hydra.constants.Constants.maxInt32),
                      (v),
                      ((stAfter)).lowLinks);
                    Integer low_w = hydra.lib.maps.FindWithDefault.apply(
                      (hydra.constants.Constants.maxInt32),
                      (w),
                      ((stAfter)).lowLinks);
                    return hydra.lib.flows.Bind.apply(
                      hydra.monads.Monads.modify((java.util.function.Function<hydra.topology.TarjanState, hydra.topology.TarjanState>) (s -> new hydra.topology.TarjanState(((s)).counter, ((s)).indices, hydra.lib.maps.Insert.apply(
                        (v),
                        hydra.lib.equality.Min.apply(
                          (low_v),
                          (low_w)),
                        ((s)).lowLinks), ((s)).stack, ((s)).onStack, ((s)).sccs))),
                      (java.util.function.Function<Boolean, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (_2 -> hydra.lib.flows.Pure.apply(true)));
                  })))),
              hydra.lib.logic.IfElse.apply(
                hydra.lib.sets.Member.apply(
                  (w),
                  ((st_)).onStack),
                ((lowLink)).apply((st_)),
                hydra.lib.flows.Pure.apply(true)))));
        });
        return hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.putState((newSt)),
          (java.util.function.Function<Boolean, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (ignored -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.MapList.apply(
              (processNeighbor),
              (neighbors)),
            (java.util.function.Function<java.util.List<Boolean>, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (_2 -> hydra.lib.flows.Bind.apply(
              hydra.monads.Monads.<hydra.topology.TarjanState>getState(),
              (java.util.function.Function<hydra.topology.TarjanState, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (stFinal -> {
                Integer idx_v = hydra.lib.maps.FindWithDefault.apply(
                  (hydra.constants.Constants.maxInt32),
                  (v),
                  ((stFinal)).indices);
                Integer low_v = hydra.lib.maps.FindWithDefault.apply(
                  (hydra.constants.Constants.maxInt32),
                  (v),
                  ((stFinal)).lowLinks);
                return hydra.lib.logic.IfElse.apply(
                  hydra.lib.equality.Equal.apply(
                    (low_v),
                    (idx_v)),
                  hydra.lib.flows.Bind.apply(
                    hydra.tarjan.Tarjan.popStackUntil((v)),
                    (java.util.function.Function<java.util.List<Integer>, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (comp -> hydra.lib.flows.Bind.apply(
                      hydra.monads.Monads.modify((java.util.function.Function<hydra.topology.TarjanState, hydra.topology.TarjanState>) (s -> new hydra.topology.TarjanState(((s)).counter, ((s)).indices, ((s)).lowLinks, ((s)).stack, ((s)).onStack, hydra.lib.lists.Cons.apply(
                        (comp),
                        ((s)).sccs)))),
                      (java.util.function.Function<Boolean, hydra.compute.Flow<hydra.topology.TarjanState, Boolean>>) (_3 -> hydra.lib.flows.Pure.apply(true))))),
                  hydra.lib.flows.Pure.apply(true));
              }))))));
      }));
  }
}
