// Note: this is an automatically generated file. Do not edit.

package hydra.tarjan;

/**
 * This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC.
 */
public interface Tarjan {
  static <T0> hydra.util.Pair<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>> adjacencyListsToGraph(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> edges0) {
    hydra.util.Lazy<java.util.List<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>>> indexedEdges = new hydra.util.Lazy<>(() -> hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_indexedEdges(hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_sortedEdges(edges0)));
    hydra.util.Lazy<java.util.Map<Integer, java.util.List<Integer>>> graph = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>, hydra.util.Pair<Integer, java.util.List<Integer>>>) (vkNeighbors -> {
        hydra.util.Lazy<Integer> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vkNeighbors));
        return (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(v.get(), hydra.lib.maybes.MapMaybe.apply(
          (java.util.function.Function<T0, hydra.util.Maybe<Integer>>) (k -> hydra.lib.maps.Lookup.apply(
            k,
            hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_keyToVertex(indexedEdges.get()))),
          hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_neighbors(hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_kNeighbors(vkNeighbors))))));
      }),
      indexedEdges.get())));
    return (hydra.util.Pair<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>>) ((hydra.util.Pair<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>>) (new hydra.util.Pair<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>>(graph.get(), (java.util.function.Function<Integer, T0>) (v1 -> hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_vertexToKey(
      hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_vertexMap(indexedEdges.get()),
      v1)))));
  }
  
  static <T0> java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> adjacencyListsToGraph_sortedEdges(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> edges0) {
    return hydra.lib.lists.SortOn.apply(
      (java.util.function.Function<hydra.util.Pair<T0, java.util.List<T0>>, T0>) ((java.util.function.Function<hydra.util.Pair<T0, java.util.List<T0>>, T0>) (hydra.lib.pairs.First::apply)),
      edges0);
  }
  
  static <T0> java.util.List<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>> adjacencyListsToGraph_indexedEdges(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> sortedEdges) {
    return hydra.lib.lists.Zip.apply(
      hydra.lib.math.Range.apply(
        0,
        hydra.lib.lists.Length.apply(sortedEdges)),
      sortedEdges);
  }
  
  static <T0> java.util.Map<T0, Integer> adjacencyListsToGraph_keyToVertex(java.util.List<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>> indexedEdges) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>, hydra.util.Pair<T0, Integer>>) (vkNeighbors -> {
        hydra.util.Lazy<Integer> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vkNeighbors));
        return (hydra.util.Pair<T0, Integer>) ((hydra.util.Pair<T0, Integer>) (new hydra.util.Pair<T0, Integer>(hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_k2(hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_kNeighbors3(vkNeighbors)), v.get())));
      }),
      indexedEdges));
  }
  
  static <T0> java.util.Map<Integer, T0> adjacencyListsToGraph_vertexMap(java.util.List<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>> indexedEdges) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>, hydra.util.Pair<Integer, T0>>) (vkNeighbors -> {
        hydra.util.Lazy<Integer> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vkNeighbors));
        return (hydra.util.Pair<Integer, T0>) ((hydra.util.Pair<Integer, T0>) (new hydra.util.Pair<Integer, T0>(v.get(), hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_k(hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph_kNeighbors2(vkNeighbors)))));
      }),
      indexedEdges));
  }
  
  static <T0> T0 adjacencyListsToGraph_vertexToKey(java.util.Map<Integer, T0> vertexMap, Integer v) {
    return hydra.lib.maybes.FromJust.apply(hydra.lib.maps.Lookup.apply(
      v,
      vertexMap));
  }
  
  static <T0> hydra.util.Pair<T0, java.util.List<T0>> adjacencyListsToGraph_kNeighbors(hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>> vkNeighbors) {
    return hydra.lib.pairs.Second.apply(vkNeighbors);
  }
  
  static <T0> java.util.List<T0> adjacencyListsToGraph_neighbors(hydra.util.Pair<T0, java.util.List<T0>> kNeighbors) {
    return hydra.lib.pairs.Second.apply(kNeighbors);
  }
  
  static <T0> hydra.util.Pair<T0, java.util.List<T0>> adjacencyListsToGraph_kNeighbors2(hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>> vkNeighbors) {
    return hydra.lib.pairs.Second.apply(vkNeighbors);
  }
  
  static <T0> T0 adjacencyListsToGraph_k(hydra.util.Pair<T0, java.util.List<T0>> kNeighbors) {
    return hydra.lib.pairs.First.apply(kNeighbors);
  }
  
  static <T0> hydra.util.Pair<T0, java.util.List<T0>> adjacencyListsToGraph_kNeighbors3(hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>> vkNeighbors) {
    return hydra.lib.pairs.Second.apply(vkNeighbors);
  }
  
  static <T0> T0 adjacencyListsToGraph_k2(hydra.util.Pair<T0, java.util.List<T0>> kNeighbors) {
    return hydra.lib.pairs.First.apply(kNeighbors);
  }
  
  static java.util.List<java.util.List<Integer>> stronglyConnectedComponents(java.util.Map<Integer, java.util.List<Integer>> graph) {
    hydra.util.Lazy<java.util.List<Integer>> verts = new hydra.util.Lazy<>(() -> hydra.lib.maps.Keys.apply(graph));
    hydra.util.Lazy<hydra.topology.TarjanState> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.topology.TarjanState, java.util.function.Function<Integer, hydra.topology.TarjanState>>) (st -> (java.util.function.Function<Integer, hydra.topology.TarjanState>) (v -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.maps.Member.apply(
          v,
          (st).indices),
        () -> st,
        () -> hydra.tarjan.Tarjan.strongConnect(
          graph,
          v,
          st)))),
      hydra.tarjan.Tarjan.initialState(),
      verts.get()));
    return hydra.lib.lists.Reverse.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>) (hydra.lib.lists.Sort::apply),
      (finalState.get()).sccs));
  }
  
  static hydra.topology.TarjanState initialState() {
    return new hydra.topology.TarjanState(0, (java.util.Map<Integer, Integer>) ((java.util.Map<Integer, Integer>) (hydra.lib.maps.Empty.<Integer, Integer>apply())), (java.util.Map<Integer, Integer>) ((java.util.Map<Integer, Integer>) (hydra.lib.maps.Empty.<Integer, Integer>apply())), (java.util.List<Integer>) (java.util.List.<Integer>of()), (java.util.Set<Integer>) (hydra.lib.sets.Empty.<Integer>apply()), (java.util.List<java.util.List<Integer>>) (java.util.List.<java.util.List<Integer>>of()));
  }
  
  static hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState> popStackUntil(Integer v, hydra.topology.TarjanState st0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<Integer>, java.util.function.Function<hydra.topology.TarjanState, hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.List<Integer>, java.util.function.Function<hydra.topology.TarjanState, hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>>>) (acc -> (java.util.function.Function<hydra.topology.TarjanState, hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>>) (st -> {
      hydra.util.Lazy<Integer> x = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply((st).stack));
      hydra.util.Lazy<java.util.List<Integer>> acc_ = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
        x.get(),
        acc));
      hydra.util.Lazy<java.util.List<Integer>> xs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply((st).stack));
      hydra.topology.TarjanState newSt = new hydra.topology.TarjanState((st).counter, (st).indices, (st).lowLinks, xs.get(), (st).onStack, (st).sccs);
      hydra.util.Lazy<hydra.topology.TarjanState> newSt2 = new hydra.util.Lazy<>(() -> new hydra.topology.TarjanState((newSt).counter, (newSt).indices, (newSt).lowLinks, (newSt).stack, hydra.lib.sets.Delete.apply(
        x.get(),
        (st).onStack), (newSt).sccs));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          x.get(),
          v),
        () -> (hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>) ((hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>) (new hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>(hydra.lib.lists.Reverse.apply(acc_.get()), newSt2.get()))),
        () -> ((go.get()).apply(acc_.get())).apply(newSt2.get()));
    })));
    return ((go.get()).apply((java.util.List<Integer>) (java.util.List.<Integer>of()))).apply(st0);
  }
  
  static hydra.topology.TarjanState strongConnect(java.util.Map<Integer, java.util.List<Integer>> graph, Integer v, hydra.topology.TarjanState st) {
    Integer i = (st).counter;
    hydra.util.Lazy<java.util.List<Integer>> neighbors = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
      () -> (java.util.List<Integer>) (java.util.List.<Integer>of()),
      v,
      graph));
    hydra.util.Lazy<hydra.topology.TarjanState> newSt = new hydra.util.Lazy<>(() -> new hydra.topology.TarjanState(hydra.lib.math.Add.apply(
      i,
      1), hydra.lib.maps.Insert.apply(
      v,
      i,
      (st).indices), hydra.lib.maps.Insert.apply(
      v,
      i,
      (st).lowLinks), hydra.lib.lists.Cons.apply(
      v,
      (st).stack), hydra.lib.sets.Insert.apply(
      v,
      (st).onStack), (st).sccs));
    java.util.function.Function<hydra.topology.TarjanState, java.util.function.Function<Integer, hydra.topology.TarjanState>> processNeighbor = (java.util.function.Function<hydra.topology.TarjanState, java.util.function.Function<Integer, hydra.topology.TarjanState>>) (st_ -> (java.util.function.Function<Integer, hydra.topology.TarjanState>) (w -> {
      java.util.function.Function<hydra.topology.TarjanState, hydra.topology.TarjanState> lowLink = (java.util.function.Function<hydra.topology.TarjanState, hydra.topology.TarjanState>) (s -> {
        hydra.util.Lazy<Integer> idx_w = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
          () -> hydra.constants.Constants.maxInt32(),
          w,
          (s).indices));
        hydra.util.Lazy<Integer> lowV1 = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
          () -> hydra.constants.Constants.maxInt32(),
          v,
          (s).lowLinks));
        return new hydra.topology.TarjanState((s).counter, (s).indices, hydra.lib.maps.Insert.apply(
          v,
          hydra.lib.equality.Min.apply(
            lowV1.get(),
            idx_w.get()),
          (s).lowLinks), (s).stack, (s).onStack, (s).sccs);
      });
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
          w,
          (st_).indices)),
        () -> ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
          hydra.topology.TarjanState stAfter = hydra.tarjan.Tarjan.strongConnect(
            graph,
            w,
            st_);
          return ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
            hydra.util.Lazy<Integer> lowV2 = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
              () -> hydra.constants.Constants.maxInt32(),
              v,
              (stAfter).lowLinks));
            return ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
              hydra.util.Lazy<Integer> low_w = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
                () -> hydra.constants.Constants.maxInt32(),
                w,
                (stAfter).lowLinks));
              return new hydra.topology.TarjanState((stAfter).counter, (stAfter).indices, hydra.lib.maps.Insert.apply(
                v,
                hydra.lib.equality.Min.apply(
                  lowV2.get(),
                  low_w.get()),
                (stAfter).lowLinks), (stAfter).stack, (stAfter).onStack, (stAfter).sccs);
            })).get();
          })).get();
        })).get(),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            w,
            (st_).onStack),
          () -> (lowLink).apply(st_),
          () -> st_));
    }));
    hydra.util.Lazy<hydra.topology.TarjanState> stAfterNeighbors = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      processNeighbor,
      newSt.get(),
      neighbors.get()));
    hydra.util.Lazy<Integer> idx_v = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
      () -> hydra.constants.Constants.maxInt32(),
      v,
      (stAfterNeighbors.get()).indices));
    hydra.util.Lazy<Integer> low_v = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
      () -> hydra.constants.Constants.maxInt32(),
      v,
      (stAfterNeighbors.get()).lowLinks));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        low_v.get(),
        idx_v.get()),
      () -> ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
        hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState> compResult = hydra.tarjan.Tarjan.popStackUntil(
          v,
          stAfterNeighbors.get());
        return ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
          hydra.util.Lazy<java.util.List<Integer>> comp = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(compResult));
          return ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
            hydra.util.Lazy<hydra.topology.TarjanState> stPopped = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(compResult));
            return new hydra.topology.TarjanState((stPopped.get()).counter, (stPopped.get()).indices, (stPopped.get()).lowLinks, (stPopped.get()).stack, (stPopped.get()).onStack, hydra.lib.lists.Cons.apply(
              comp.get(),
              (stPopped.get()).sccs));
          })).get();
        })).get();
      })).get(),
      () -> stAfterNeighbors.get());
  }
}
