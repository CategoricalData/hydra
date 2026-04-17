// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Utilities for sorting. This module includes an implementation of Tarjan's algorithm, originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC.
 */
public interface Sorting {
  static <T0, T1> java.util.Map<T0, java.util.List<T1>> adjacencyListToMap(java.util.List<hydra.util.Pair<T0, java.util.List<T1>>> pairs) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<T0, java.util.List<T1>>, java.util.function.Function<hydra.util.Pair<T0, java.util.List<T1>>, java.util.Map<T0, java.util.List<T1>>>>) (mp -> (java.util.function.Function<hydra.util.Pair<T0, java.util.List<T1>>, java.util.Map<T0, java.util.List<T1>>>) (p -> {
        hydra.util.Lazy<T0> k = new hydra.util.Lazy<>(() -> hydra.Sorting.<T0, T1>adjacencyListToMap_k(p));
        return hydra.lib.maps.Insert.apply(
          k.get(),
          hydra.lib.lists.Concat2.apply(
            hydra.Sorting.<T0, T1>adjacencyListToMap_existing(
              k.get(),
              mp),
            hydra.Sorting.<T0, T1>adjacencyListToMap_vs(p)),
          mp);
      })),
      (java.util.Map<T0, java.util.List<T1>>) ((java.util.Map<T0, java.util.List<T1>>) (hydra.lib.maps.Empty.<T0, java.util.List<T1>>apply())),
      pairs);
  }

  static <T0, T1> java.util.List<T1> adjacencyListToMap_existing(T0 k, java.util.Map<T0, java.util.List<T1>> mp) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<T1>) (java.util.Collections.<T1>emptyList()),
      (java.util.function.Function<java.util.List<T1>, java.util.List<T1>>) (hydra.lib.equality.Identity::apply),
      hydra.lib.maps.Lookup.apply(
        k,
        mp));
  }

  static <T0, T1> T0 adjacencyListToMap_k(hydra.util.Pair<T0, java.util.List<T1>> p) {
    return hydra.lib.pairs.First.apply(p);
  }

  static <T0, T1> java.util.List<T1> adjacencyListToMap_vs(hydra.util.Pair<T0, java.util.List<T1>> p) {
    return hydra.lib.pairs.Second.apply(p);
  }

  static <T0> hydra.util.Pair<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, hydra.util.Maybe<T0>>> adjacencyListsToGraph(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> edges0) {
    hydra.util.Lazy<java.util.List<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>>> indexedEdges = new hydra.util.Lazy<>(() -> hydra.Sorting.<T0>adjacencyListsToGraph_indexedEdges(hydra.Sorting.<T0>adjacencyListsToGraph_sortedEdges(edges0)));
    hydra.util.Lazy<java.util.Map<Integer, java.util.List<Integer>>> graph = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>, hydra.util.Pair<Integer, java.util.List<Integer>>>) (vkNeighbors -> {
        hydra.util.Lazy<Integer> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vkNeighbors));
        return (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(v.get(), hydra.lib.maybes.MapMaybe.apply(
          (java.util.function.Function<T0, hydra.util.Maybe<Integer>>) (k -> hydra.lib.maps.Lookup.apply(
            k,
            hydra.Sorting.<T0>adjacencyListsToGraph_keyToVertex(indexedEdges.get()))),
          hydra.Sorting.<T0>adjacencyListsToGraph_neighbors(hydra.Sorting.<T0>adjacencyListsToGraph_kNeighbors(vkNeighbors))))));
      }),
      indexedEdges.get())));
    return (hydra.util.Pair<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, hydra.util.Maybe<T0>>>) ((hydra.util.Pair<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, hydra.util.Maybe<T0>>>) (new hydra.util.Pair<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, hydra.util.Maybe<T0>>>(graph.get(), (java.util.function.Function<Integer, hydra.util.Maybe<T0>>) (v1 -> hydra.Sorting.<T0>adjacencyListsToGraph_vertexToKey(
      hydra.Sorting.<T0>adjacencyListsToGraph_vertexMap(indexedEdges.get()),
      v1)))));
  }

  static <T0> java.util.List<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>> adjacencyListsToGraph_indexedEdges(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> sortedEdges) {
    return hydra.lib.lists.Zip.apply(
      hydra.lib.math.Range.apply(
        0,
        hydra.lib.lists.Length.apply(sortedEdges)),
      sortedEdges);
  }

  static <T0> T0 adjacencyListsToGraph_k(hydra.util.Pair<T0, java.util.List<T0>> kNeighbors) {
    return hydra.lib.pairs.First.apply(kNeighbors);
  }

  static <T0> T0 adjacencyListsToGraph_k2(hydra.util.Pair<T0, java.util.List<T0>> kNeighbors) {
    return hydra.lib.pairs.First.apply(kNeighbors);
  }

  static <T0> hydra.util.Pair<T0, java.util.List<T0>> adjacencyListsToGraph_kNeighbors(hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>> vkNeighbors) {
    return hydra.lib.pairs.Second.apply(vkNeighbors);
  }

  static <T0> hydra.util.Pair<T0, java.util.List<T0>> adjacencyListsToGraph_kNeighbors2(hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>> vkNeighbors) {
    return hydra.lib.pairs.Second.apply(vkNeighbors);
  }

  static <T0> hydra.util.Pair<T0, java.util.List<T0>> adjacencyListsToGraph_kNeighbors3(hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>> vkNeighbors) {
    return hydra.lib.pairs.Second.apply(vkNeighbors);
  }

  static <T0> java.util.Map<T0, Integer> adjacencyListsToGraph_keyToVertex(java.util.List<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>> indexedEdges) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>, hydra.util.Pair<T0, Integer>>) (vkNeighbors -> {
        hydra.util.Lazy<Integer> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vkNeighbors));
        return (hydra.util.Pair<T0, Integer>) ((hydra.util.Pair<T0, Integer>) (new hydra.util.Pair<T0, Integer>(hydra.Sorting.<T0>adjacencyListsToGraph_k2(hydra.Sorting.<T0>adjacencyListsToGraph_kNeighbors3(vkNeighbors)), v.get())));
      }),
      indexedEdges));
  }

  static <T0> java.util.List<T0> adjacencyListsToGraph_neighbors(hydra.util.Pair<T0, java.util.List<T0>> kNeighbors) {
    return hydra.lib.pairs.Second.apply(kNeighbors);
  }

  static <T0> java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> adjacencyListsToGraph_sortedEdges(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> edges0) {
    return hydra.lib.lists.SortOn.apply(
      (java.util.function.Function<hydra.util.Pair<T0, java.util.List<T0>>, T0>) ((java.util.function.Function<hydra.util.Pair<T0, java.util.List<T0>>, T0>) (hydra.lib.pairs.First::apply)),
      edges0);
  }

  static <T0> java.util.Map<Integer, T0> adjacencyListsToGraph_vertexMap(java.util.List<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>> indexedEdges) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Pair<T0, java.util.List<T0>>>, hydra.util.Pair<Integer, T0>>) (vkNeighbors -> {
        hydra.util.Lazy<Integer> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vkNeighbors));
        return (hydra.util.Pair<Integer, T0>) ((hydra.util.Pair<Integer, T0>) (new hydra.util.Pair<Integer, T0>(v.get(), hydra.Sorting.<T0>adjacencyListsToGraph_k(hydra.Sorting.<T0>adjacencyListsToGraph_kNeighbors2(vkNeighbors)))));
      }),
      indexedEdges));
  }

  static <T0> hydra.util.Maybe<T0> adjacencyListsToGraph_vertexToKey(java.util.Map<Integer, T0> vertexMap, Integer v) {
    return hydra.lib.maps.Lookup.apply(
      v,
      vertexMap);
  }

  static <T0, T1> hydra.topology.OrderingIsomorphism<T1> createOrderingIsomorphism(java.util.List<T0> sourceOrd, java.util.List<T0> targetOrd) {
    return (hydra.topology.OrderingIsomorphism<T1>) (new hydra.topology.OrderingIsomorphism<T1>((java.util.function.Function<java.util.List<T1>, java.util.List<T1>>) (v1 -> hydra.Sorting.<T0, T1>createOrderingIsomorphism_sourceToTargetMapping(
      sourceOrd,
      targetOrd,
      v1)), (java.util.function.Function<java.util.List<T1>, java.util.List<T1>>) (v1 -> hydra.Sorting.<T0, T1>createOrderingIsomorphism_targetToSourceMapping(
      sourceOrd,
      targetOrd,
      v1))));
  }

  static <T0, T2> java.util.Map<T0, T2> createOrderingIsomorphism_mp(java.util.List<T2> els, java.util.List<T0> targetOrd) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      targetOrd,
      els));
  }

  static <T0, T2> java.util.Map<T0, T2> createOrderingIsomorphism_mp2(java.util.List<T2> els, java.util.List<T0> sourceOrd) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      sourceOrd,
      els));
  }

  static <T0, T2> java.util.List<T2> createOrderingIsomorphism_sourceToTargetMapping(java.util.List<T0> sourceOrd, java.util.List<T0> targetOrd, java.util.List<T2> els) {
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Maybe<T2>>) (n -> hydra.lib.maps.Lookup.apply(
        n,
        hydra.Sorting.<T0, T2>createOrderingIsomorphism_mp2(
          els,
          sourceOrd))),
      targetOrd));
  }

  static <T0, T2> java.util.List<T2> createOrderingIsomorphism_targetToSourceMapping(java.util.List<T0> sourceOrd, java.util.List<T0> targetOrd, java.util.List<T2> els) {
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Maybe<T2>>) (n -> hydra.lib.maps.Lookup.apply(
        n,
        hydra.Sorting.<T0, T2>createOrderingIsomorphism_mp(
          els,
          targetOrd))),
      sourceOrd));
  }

  static <T0> java.util.Set<T0> findReachableNodes(java.util.function.Function<T0, java.util.Set<T0>> adj, T0 root) {
    return hydra.Sorting.<T0>findReachableNodes_visit(
      adj,
      hydra.lib.sets.Singleton.apply(root),
      root);
  }

  static <T0> java.util.Set<T0> findReachableNodes_toVisit(java.util.function.Function<T0, java.util.Set<T0>> adj, T0 node, java.util.Set<T0> visited) {
    return hydra.lib.sets.Difference.apply(
      (adj).apply(node),
      visited);
  }

  static <T0> java.util.Set<T0> findReachableNodes_visit(java.util.function.Function<T0, java.util.Set<T0>> adj, java.util.Set<T0> visited, T0 node) {
    hydra.util.Lazy<java.util.Set<T0>> toVisit = new hydra.util.Lazy<>(() -> hydra.Sorting.<T0>findReachableNodes_toVisit(
      adj,
      node,
      visited));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(toVisit.get()),
      () -> visited,
      () -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<java.util.Set<T0>, java.util.function.Function<T0, java.util.Set<T0>>>) (v -> (java.util.function.Function<T0, java.util.Set<T0>>) (n -> hydra.Sorting.<T0>findReachableNodes_visit(
          adj,
          hydra.lib.sets.Insert.apply(
            n,
            v),
          n))),
        visited,
        hydra.lib.sets.ToList.apply(toVisit.get())));
  }

  static hydra.topology.TarjanState initialState() {
    return new hydra.topology.TarjanState(0, (java.util.Map<Integer, Integer>) ((java.util.Map<Integer, Integer>) (hydra.lib.maps.Empty.<Integer, Integer>apply())), (java.util.Map<Integer, Integer>) ((java.util.Map<Integer, Integer>) (hydra.lib.maps.Empty.<Integer, Integer>apply())), (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()), (java.util.Set<Integer>) (hydra.lib.sets.Empty.<Integer>apply()), (java.util.List<java.util.List<Integer>>) (java.util.Collections.<java.util.List<Integer>>emptyList()));
  }

  static hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState> popStackUntil(Integer v, hydra.topology.TarjanState st0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<Integer>, java.util.function.Function<hydra.topology.TarjanState, hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.List<Integer>, java.util.function.Function<hydra.topology.TarjanState, hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>>>) (acc -> (java.util.function.Function<hydra.topology.TarjanState, hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>>) (st -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>) ((hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>) (new hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>(hydra.lib.lists.Reverse.apply(acc), st))),
      (java.util.function.Function<hydra.util.Pair<Integer, java.util.List<Integer>>, hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>>) (uc -> {
        hydra.util.Lazy<Integer> x = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(uc));
        hydra.util.Lazy<java.util.List<Integer>> acc_ = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
          x.get(),
          acc));
        hydra.util.Lazy<java.util.List<Integer>> xs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(uc));
        hydra.topology.TarjanState newSt = new hydra.topology.TarjanState((st).counter, (st).indices, (st).lowLinks, xs.get(), (st).onStack, (st).sccs);
        hydra.util.Lazy<hydra.topology.TarjanState> newSt2 = new hydra.util.Lazy<>(() -> new hydra.topology.TarjanState((newSt).counter, (newSt).indices, (newSt).lowLinks, (newSt).stack, hydra.lib.sets.Delete.apply(
          x.get(),
          (st).onStack), (newSt).sccs));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            x.get(),
            v),
          () -> (hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>) ((hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>) (new hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState>(hydra.lib.lists.Reverse.apply(acc_.get()), newSt2.get()))),
          () -> go.get().apply(acc_.get()).apply(newSt2.get()));
      }),
      hydra.lib.lists.Uncons.apply((st).stack)))));
    return go.get().apply((java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())).apply(st0);
  }

  static <T0, T1> java.util.List<hydra.util.Pair<T0, java.util.Set<T1>>> propagateTags(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> edges, java.util.List<hydra.util.Pair<T0, java.util.List<T1>>> nodeTags) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Pair<T0, java.util.Set<T1>>>) (n -> (hydra.util.Pair<T0, java.util.Set<T1>>) ((hydra.util.Pair<T0, java.util.Set<T1>>) (new hydra.util.Pair<T0, java.util.Set<T1>>(n, hydra.Sorting.<T0, T1>propagateTags_getTagsForNode(
        hydra.Sorting.<T0>propagateTags_adjMap(edges),
        hydra.Sorting.<T0, T1>propagateTags_tagMap(nodeTags),
        n))))),
      hydra.Sorting.<T0, T1>propagateTags_allNodes(
        edges,
        nodeTags));
  }

  static <T0> java.util.Map<T0, java.util.List<T0>> propagateTags_adjMap(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> edges) {
    return hydra.Sorting.<T0, T0>adjacencyListToMap(edges);
  }

  static <T0, T1> java.util.List<T0> propagateTags_allNodes(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> edges, java.util.List<hydra.util.Pair<T0, java.util.List<T1>>> nodeTags) {
    return hydra.lib.sets.ToList.apply(hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat2.apply(
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<T0, java.util.List<T0>>, T0>) ((java.util.function.Function<hydra.util.Pair<T0, java.util.List<T0>>, T0>) (hydra.lib.pairs.First::apply)),
        edges),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<T0, java.util.List<T1>>, T0>) ((java.util.function.Function<hydra.util.Pair<T0, java.util.List<T1>>, T0>) (hydra.lib.pairs.First::apply)),
        nodeTags))));
  }

  static <T0, T1> java.util.Set<T1> propagateTags_getTagsForNode(java.util.Map<T0, java.util.List<T0>> adjMap, java.util.Map<T0, java.util.Set<T1>> tagMap, T0 node) {
    return hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, java.util.Set<T1>>) (n -> hydra.lib.maybes.Maybe.applyLazy(
        () -> (java.util.Set<T1>) (hydra.lib.sets.Empty.<T1>apply()),
        (java.util.function.Function<java.util.Set<T1>, java.util.Set<T1>>) (hydra.lib.equality.Identity::apply),
        hydra.lib.maps.Lookup.apply(
          n,
          tagMap))),
      hydra.lib.sets.ToList.apply(hydra.Sorting.<T0>propagateTags_reachable(
        adjMap,
        node))));
  }

  static <T0> java.util.Set<T0> propagateTags_reachable(java.util.Map<T0, java.util.List<T0>> adjMap, T0 node) {
    return hydra.Sorting.<T0>findReachableNodes(
      (java.util.function.Function<T0, java.util.Set<T0>>) (n -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Maybe.applyLazy(
        () -> (java.util.List<T0>) (java.util.Collections.<T0>emptyList()),
        (java.util.function.Function<java.util.List<T0>, java.util.List<T0>>) (hydra.lib.equality.Identity::apply),
        hydra.lib.maps.Lookup.apply(
          n,
          adjMap)))),
      node);
  }

  static <T0, T1> java.util.Map<T0, java.util.Set<T1>> propagateTags_tagMap(java.util.List<hydra.util.Pair<T0, java.util.List<T1>>> nodeTags) {
    return hydra.lib.maps.Map.apply(
      (java.util.function.Function<java.util.List<T1>, java.util.Set<T1>>) (hydra.lib.sets.FromList::apply),
      hydra.Sorting.<T0, T1>adjacencyListToMap(nodeTags));
  }

  static hydra.topology.TarjanState strongConnect(java.util.Map<Integer, java.util.List<Integer>> graph, Integer v, hydra.topology.TarjanState st) {
    Integer i = (st).counter;
    hydra.util.Lazy<java.util.List<Integer>> neighbors = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
      () -> (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()),
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
          () -> hydra.Constants.maxInt32(),
          w,
          (s).indices));
        hydra.util.Lazy<Integer> lowV1 = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
          () -> hydra.Constants.maxInt32(),
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
          hydra.topology.TarjanState stAfter = hydra.Sorting.strongConnect(
            graph,
            w,
            st_);
          return ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
            hydra.util.Lazy<Integer> lowV2 = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
              () -> hydra.Constants.maxInt32(),
              v,
              (stAfter).lowLinks));
            return ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
              hydra.util.Lazy<Integer> low_w = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
                () -> hydra.Constants.maxInt32(),
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
      () -> hydra.Constants.maxInt32(),
      v,
      stAfterNeighbors.get().indices));
    hydra.util.Lazy<Integer> low_v = new hydra.util.Lazy<>(() -> hydra.lib.maps.FindWithDefault.applyLazy(
      () -> hydra.Constants.maxInt32(),
      v,
      stAfterNeighbors.get().lowLinks));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        low_v.get(),
        idx_v.get()),
      () -> ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
        hydra.util.Pair<java.util.List<Integer>, hydra.topology.TarjanState> compResult = hydra.Sorting.popStackUntil(
          v,
          stAfterNeighbors.get());
        return ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
          hydra.util.Lazy<java.util.List<Integer>> comp = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(compResult));
          return ((java.util.function.Supplier<hydra.topology.TarjanState>) (() -> {
            hydra.util.Lazy<hydra.topology.TarjanState> stPopped = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(compResult));
            return new hydra.topology.TarjanState(stPopped.get().counter, stPopped.get().indices, stPopped.get().lowLinks, stPopped.get().stack, stPopped.get().onStack, hydra.lib.lists.Cons.apply(
              comp.get(),
              stPopped.get().sccs));
          })).get();
        })).get();
      })).get(),
      () -> stAfterNeighbors.get());
  }

  static java.util.List<java.util.List<Integer>> stronglyConnectedComponents(java.util.Map<Integer, java.util.List<Integer>> graph) {
    hydra.util.Lazy<java.util.List<Integer>> verts = new hydra.util.Lazy<>(() -> hydra.lib.maps.Keys.apply(graph));
    hydra.util.Lazy<hydra.topology.TarjanState> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.topology.TarjanState, java.util.function.Function<Integer, hydra.topology.TarjanState>>) (st -> (java.util.function.Function<Integer, hydra.topology.TarjanState>) (v -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.maps.Member.apply(
          v,
          (st).indices),
        () -> st,
        () -> hydra.Sorting.strongConnect(
          graph,
          v,
          st)))),
      hydra.Sorting.initialState(),
      verts.get()));
    return hydra.lib.lists.Reverse.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>) (hydra.lib.lists.Sort::apply),
      finalState.get().sccs));
  }

  static <T0> hydra.util.Either<java.util.List<java.util.List<T0>>, java.util.List<T0>> topologicalSort(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> pairs) {
    hydra.util.Lazy<java.util.List<java.util.List<T0>>> sccs = new hydra.util.Lazy<>(() -> hydra.Sorting.<T0>topologicalSort_sccs(pairs));
    hydra.util.Lazy<java.util.List<java.util.List<T0>>> withCycles = new hydra.util.Lazy<>(() -> hydra.Sorting.<T0>topologicalSort_withCycles(sccs.get()));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(withCycles.get()),
      () -> hydra.util.Either.<java.util.List<java.util.List<T0>>, java.util.List<T0>>right(hydra.lib.lists.Concat.apply(sccs.get())),
      () -> hydra.util.Either.<java.util.List<java.util.List<T0>>, java.util.List<T0>>left(withCycles.get()));
  }

  static <T0> java.util.List<java.util.List<T0>> topologicalSortComponents(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> pairs) {
    hydra.util.Lazy<hydra.util.Pair<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, hydra.util.Maybe<T0>>>> graphResult = new hydra.util.Lazy<>(() -> hydra.Sorting.<T0>topologicalSortComponents_graphResult(pairs));
    hydra.util.Lazy<java.util.Map<Integer, java.util.List<Integer>>> g = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(graphResult.get()));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<Integer>, java.util.List<T0>>) (comp -> hydra.lib.maybes.MapMaybe.apply(
        hydra.lib.pairs.Second.apply(graphResult.get()),
        comp)),
      hydra.Sorting.stronglyConnectedComponents(g.get()));
  }

  static <T0> hydra.util.Pair<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, hydra.util.Maybe<T0>>> topologicalSortComponents_graphResult(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> pairs) {
    return hydra.Sorting.<T0>adjacencyListsToGraph(pairs);
  }

  static <T0, T1> java.util.List<java.util.List<T0>> topologicalSortNodes(java.util.function.Function<T0, T1> getKey, java.util.function.Function<T0, java.util.List<T1>> getAdj, java.util.List<T0> nodes) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<T1>, java.util.List<T0>>) (c -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<T1, hydra.util.Maybe<T0>>) (k -> hydra.lib.maps.Lookup.apply(
          k,
          hydra.Sorting.<T0, T1>topologicalSortNodes_nodesByKey(
            getKey,
            nodes))),
        c))),
      hydra.Sorting.<T1>topologicalSortNodes_comps(hydra.Sorting.<T0, T1>topologicalSortNodes_pairs(
        getAdj,
        getKey,
        nodes)));
  }

  static <T1> java.util.List<java.util.List<T1>> topologicalSortNodes_comps(java.util.List<hydra.util.Pair<T1, java.util.List<T1>>> pairs) {
    return hydra.Sorting.<T1>topologicalSortComponents(pairs);
  }

  static <T0, T1> java.util.Map<T1, T0> topologicalSortNodes_nodesByKey(java.util.function.Function<T0, T1> getKey, java.util.List<T0> nodes) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Pair<T1, T0>>) (n -> (hydra.util.Pair<T1, T0>) ((hydra.util.Pair<T1, T0>) (new hydra.util.Pair<T1, T0>((getKey).apply(n), n)))),
      nodes));
  }

  static <T0, T1> java.util.List<hydra.util.Pair<T1, java.util.List<T1>>> topologicalSortNodes_pairs(java.util.function.Function<T0, java.util.List<T1>> getAdj, java.util.function.Function<T0, T1> getKey, java.util.List<T0> nodes) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Pair<T1, java.util.List<T1>>>) (n -> (hydra.util.Pair<T1, java.util.List<T1>>) ((hydra.util.Pair<T1, java.util.List<T1>>) (new hydra.util.Pair<T1, java.util.List<T1>>((getKey).apply(n), (getAdj).apply(n))))),
      nodes);
  }

  static <T1> Boolean topologicalSort_isCycle(java.util.List<T1> scc) {
    return hydra.lib.equality.Gt.apply(
      hydra.lib.lists.Length.apply(scc),
      1);
  }

  static <T0> java.util.List<java.util.List<T0>> topologicalSort_sccs(java.util.List<hydra.util.Pair<T0, java.util.List<T0>>> pairs) {
    return hydra.Sorting.<T0>topologicalSortComponents(pairs);
  }

  static <T0> java.util.List<java.util.List<T0>> topologicalSort_withCycles(java.util.List<java.util.List<T0>> sccs) {
    return hydra.lib.lists.Filter.apply(
      p0 -> hydra.Sorting.<T0>topologicalSort_isCycle(p0),
      sccs);
  }
}
