// Note: this is an automatically generated file. Do not edit.

package hydra.sorting;

/**
 * Utilities for sorting.
 */
public interface Sorting {
  static <T0, T1> hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>> adjacencyListToMap(hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T1>>> pairs) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>>, java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T1>>, hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>>>>) (mp -> (java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T1>>, hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>>>) (p -> {
        hydra.util.Lazy<T0> k = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.<T0, T1>adjacencyListToMap_k(p));
        return hydra.lib.maps.Insert.apply(
          k.get(),
          hydra.lib.lists.Concat2.apply(
            hydra.sorting.Sorting.<T0, T1>adjacencyListToMap_existing(
              k.get(),
              mp),
            hydra.sorting.Sorting.<T0, T1>adjacencyListToMap_vs(p)),
          mp);
      })),
      (hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>>) ((hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>>) (hydra.lib.maps.Empty.<T0, hydra.util.ConsList<T1>>apply())),
      pairs);
  }

  static <T0, T1> T0 adjacencyListToMap_k(hydra.util.Pair<T0, hydra.util.ConsList<T1>> p) {
    return hydra.lib.pairs.First.apply(p);
  }

  static <T0, T1> hydra.util.ConsList<T1> adjacencyListToMap_vs(hydra.util.Pair<T0, hydra.util.ConsList<T1>> p) {
    return hydra.lib.pairs.Second.apply(p);
  }

  static <T0, T1> hydra.util.ConsList<T1> adjacencyListToMap_existing(T0 k, hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>> mp) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty()),
      (java.util.function.Function<hydra.util.ConsList<T1>, hydra.util.ConsList<T1>>) (hydra.lib.equality.Identity::apply),
      hydra.lib.maps.Lookup.apply(
        k,
        mp));
  }

  static <T0, T1> hydra.topology.OrderingIsomorphism<T1> createOrderingIsomorphism(hydra.util.ConsList<T0> sourceOrd, hydra.util.ConsList<T0> targetOrd) {
    return (hydra.topology.OrderingIsomorphism<T1>) (new hydra.topology.OrderingIsomorphism<T1>((java.util.function.Function<hydra.util.ConsList<T1>, hydra.util.ConsList<T1>>) (v1 -> hydra.sorting.Sorting.<T0, T1>createOrderingIsomorphism_sourceToTargetMapping(
      sourceOrd,
      targetOrd,
      v1)), (java.util.function.Function<hydra.util.ConsList<T1>, hydra.util.ConsList<T1>>) (v1 -> hydra.sorting.Sorting.<T0, T1>createOrderingIsomorphism_targetToSourceMapping(
      sourceOrd,
      targetOrd,
      v1))));
  }

  static <T0, T2> hydra.util.ConsList<T2> createOrderingIsomorphism_sourceToTargetMapping(hydra.util.ConsList<T0> sourceOrd, hydra.util.ConsList<T0> targetOrd, hydra.util.ConsList<T2> els) {
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Maybe<T2>>) (n -> hydra.lib.maps.Lookup.apply(
        n,
        hydra.sorting.Sorting.<T0, T2>createOrderingIsomorphism_mp2(
          els,
          sourceOrd))),
      targetOrd));
  }

  static <T0, T2> hydra.util.ConsList<T2> createOrderingIsomorphism_targetToSourceMapping(hydra.util.ConsList<T0> sourceOrd, hydra.util.ConsList<T0> targetOrd, hydra.util.ConsList<T2> els) {
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Maybe<T2>>) (n -> hydra.lib.maps.Lookup.apply(
        n,
        hydra.sorting.Sorting.<T0, T2>createOrderingIsomorphism_mp(
          els,
          targetOrd))),
      sourceOrd));
  }

  static <T0, T2> hydra.util.PersistentMap<T0, T2> createOrderingIsomorphism_mp(hydra.util.ConsList<T2> els, hydra.util.ConsList<T0> targetOrd) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      targetOrd,
      els));
  }

  static <T0, T2> hydra.util.PersistentMap<T0, T2> createOrderingIsomorphism_mp2(hydra.util.ConsList<T2> els, hydra.util.ConsList<T0> sourceOrd) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      sourceOrd,
      els));
  }

  static <T0> hydra.util.PersistentSet<T0> findReachableNodes(java.util.function.Function<T0, hydra.util.PersistentSet<T0>> adj, T0 root) {
    return hydra.sorting.Sorting.<T0>findReachableNodes_visit(
      adj,
      hydra.lib.sets.Singleton.apply(root),
      root);
  }

  static <T0> hydra.util.PersistentSet<T0> findReachableNodes_visit(java.util.function.Function<T0, hydra.util.PersistentSet<T0>> adj, hydra.util.PersistentSet<T0> visited, T0 node) {
    hydra.util.Lazy<hydra.util.PersistentSet<T0>> toVisit = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.<T0>findReachableNodes_toVisit(
      adj,
      node,
      visited));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(toVisit.get()),
      () -> visited,
      () -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.util.PersistentSet<T0>, java.util.function.Function<T0, hydra.util.PersistentSet<T0>>>) (v -> (java.util.function.Function<T0, hydra.util.PersistentSet<T0>>) (n -> hydra.sorting.Sorting.<T0>findReachableNodes_visit(
          adj,
          hydra.lib.sets.Insert.apply(
            n,
            v),
          n))),
        visited,
        hydra.lib.sets.ToList.apply(toVisit.get())));
  }

  static <T0> hydra.util.PersistentSet<T0> findReachableNodes_toVisit(java.util.function.Function<T0, hydra.util.PersistentSet<T0>> adj, T0 node, hydra.util.PersistentSet<T0> visited) {
    return hydra.lib.sets.Difference.apply(
      (adj).apply(node),
      visited);
  }

  static <T0, T1> hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.PersistentSet<T1>>> propagateTags(hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>> edges, hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T1>>> nodeTags) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Pair<T0, hydra.util.PersistentSet<T1>>>) (n -> (hydra.util.Pair<T0, hydra.util.PersistentSet<T1>>) ((hydra.util.Pair<T0, hydra.util.PersistentSet<T1>>) (new hydra.util.Pair<T0, hydra.util.PersistentSet<T1>>(n, hydra.sorting.Sorting.<T0, T1>propagateTags_getTagsForNode(
        hydra.sorting.Sorting.<T0>propagateTags_adjMap(edges),
        hydra.sorting.Sorting.<T0, T1>propagateTags_tagMap(nodeTags),
        n))))),
      hydra.sorting.Sorting.<T0, T1>propagateTags_allNodes(
        edges,
        nodeTags));
  }

  static <T0> hydra.util.PersistentMap<T0, hydra.util.ConsList<T0>> propagateTags_adjMap(hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>> edges) {
    return hydra.sorting.Sorting.<T0, T0>adjacencyListToMap(edges);
  }

  static <T0, T1> hydra.util.PersistentMap<T0, hydra.util.PersistentSet<T1>> propagateTags_tagMap(hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T1>>> nodeTags) {
    return hydra.lib.maps.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<T1>, hydra.util.PersistentSet<T1>>) (hydra.lib.sets.FromList::apply),
      hydra.sorting.Sorting.<T0, T1>adjacencyListToMap(nodeTags));
  }

  static <T0, T1> hydra.util.ConsList<T0> propagateTags_allNodes(hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>> edges, hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T1>>> nodeTags) {
    return hydra.lib.sets.ToList.apply(hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat2.apply(
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T0>>, T0>) ((java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T0>>, T0>) (hydra.lib.pairs.First::apply)),
        edges),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T1>>, T0>) ((java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T1>>, T0>) (hydra.lib.pairs.First::apply)),
        nodeTags))));
  }

  static <T0, T1> hydra.util.PersistentSet<T1> propagateTags_getTagsForNode(hydra.util.PersistentMap<T0, hydra.util.ConsList<T0>> adjMap, hydra.util.PersistentMap<T0, hydra.util.PersistentSet<T1>> tagMap, T0 node) {
    return hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.PersistentSet<T1>>) (n -> hydra.lib.maybes.Maybe.applyLazy(
        () -> (hydra.util.PersistentSet<T1>) (hydra.lib.sets.Empty.<T1>apply()),
        (java.util.function.Function<hydra.util.PersistentSet<T1>, hydra.util.PersistentSet<T1>>) (hydra.lib.equality.Identity::apply),
        hydra.lib.maps.Lookup.apply(
          n,
          tagMap))),
      hydra.lib.sets.ToList.apply(hydra.sorting.Sorting.<T0>propagateTags_reachable(
        adjMap,
        node))));
  }

  static <T0> hydra.util.PersistentSet<T0> propagateTags_reachable(hydra.util.PersistentMap<T0, hydra.util.ConsList<T0>> adjMap, T0 node) {
    return hydra.sorting.Sorting.<T0>findReachableNodes(
      (java.util.function.Function<T0, hydra.util.PersistentSet<T0>>) (n -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Maybe.applyLazy(
        () -> (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),
        (java.util.function.Function<hydra.util.ConsList<T0>, hydra.util.ConsList<T0>>) (hydra.lib.equality.Identity::apply),
        hydra.lib.maps.Lookup.apply(
          n,
          adjMap)))),
      node);
  }

  static <T0> hydra.util.Either<hydra.util.ConsList<hydra.util.ConsList<T0>>, hydra.util.ConsList<T0>> topologicalSort(hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>> pairs) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.ConsList<T0>>> sccs = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.<T0>topologicalSort_sccs(pairs));
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.ConsList<T0>>> withCycles = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.<T0>topologicalSort_withCycles(sccs.get()));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(withCycles.get()),
      () -> hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<T0>>, hydra.util.ConsList<T0>>right(hydra.lib.lists.Concat.apply(sccs.get())),
      () -> hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<T0>>, hydra.util.ConsList<T0>>left(withCycles.get()));
  }

  static <T0> hydra.util.ConsList<hydra.util.ConsList<T0>> topologicalSort_sccs(hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>> pairs) {
    return hydra.sorting.Sorting.<T0>topologicalSortComponents(pairs);
  }

  static <T1> Boolean topologicalSort_isCycle(hydra.util.ConsList<T1> scc) {
    return hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply(scc)));
  }

  static <T0> hydra.util.ConsList<hydra.util.ConsList<T0>> topologicalSort_withCycles(hydra.util.ConsList<hydra.util.ConsList<T0>> sccs) {
    return hydra.lib.lists.Filter.apply(
      p0 -> hydra.sorting.Sorting.<T0>topologicalSort_isCycle(p0),
      sccs);
  }

  static <T0> hydra.util.ConsList<hydra.util.ConsList<T0>> topologicalSortComponents(hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>> pairs) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.PersistentMap<Integer, hydra.util.ConsList<Integer>>, java.util.function.Function<Integer, T0>>> graphResult = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.<T0>topologicalSortComponents_graphResult(pairs));
    hydra.util.Lazy<hydra.util.PersistentMap<Integer, hydra.util.ConsList<Integer>>> g = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(graphResult.get()));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<Integer>, hydra.util.ConsList<T0>>) (comp -> hydra.lib.lists.Map.apply(
        hydra.lib.pairs.Second.apply(graphResult.get()),
        comp)),
      hydra.tarjan.Tarjan.stronglyConnectedComponents(g.get()));
  }

  static <T0> hydra.util.Pair<hydra.util.PersistentMap<Integer, hydra.util.ConsList<Integer>>, java.util.function.Function<Integer, T0>> topologicalSortComponents_graphResult(hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>> pairs) {
    return hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph(pairs);
  }

  static <T0, T1> hydra.util.ConsList<hydra.util.ConsList<T0>> topologicalSortNodes(java.util.function.Function<T0, T1> getKey, java.util.function.Function<T0, hydra.util.ConsList<T1>> getAdj, hydra.util.ConsList<T0> nodes) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<T1>, hydra.util.ConsList<T0>>) (c -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<T1, hydra.util.Maybe<T0>>) (k -> hydra.lib.maps.Lookup.apply(
          k,
          hydra.sorting.Sorting.<T0, T1>topologicalSortNodes_nodesByKey(
            getKey,
            nodes))),
        c))),
      hydra.sorting.Sorting.<T1>topologicalSortNodes_comps(hydra.sorting.Sorting.<T0, T1>topologicalSortNodes_pairs(
        getAdj,
        getKey,
        nodes)));
  }

  static <T0, T1> hydra.util.PersistentMap<T1, T0> topologicalSortNodes_nodesByKey(java.util.function.Function<T0, T1> getKey, hydra.util.ConsList<T0> nodes) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Pair<T1, T0>>) (n -> (hydra.util.Pair<T1, T0>) ((hydra.util.Pair<T1, T0>) (new hydra.util.Pair<T1, T0>((getKey).apply(n), n)))),
      nodes));
  }

  static <T0, T1> hydra.util.ConsList<hydra.util.Pair<T1, hydra.util.ConsList<T1>>> topologicalSortNodes_pairs(java.util.function.Function<T0, hydra.util.ConsList<T1>> getAdj, java.util.function.Function<T0, T1> getKey, hydra.util.ConsList<T0> nodes) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Pair<T1, hydra.util.ConsList<T1>>>) (n -> (hydra.util.Pair<T1, hydra.util.ConsList<T1>>) ((hydra.util.Pair<T1, hydra.util.ConsList<T1>>) (new hydra.util.Pair<T1, hydra.util.ConsList<T1>>((getKey).apply(n), (getAdj).apply(n))))),
      nodes);
  }

  static <T1> hydra.util.ConsList<hydra.util.ConsList<T1>> topologicalSortNodes_comps(hydra.util.ConsList<hydra.util.Pair<T1, hydra.util.ConsList<T1>>> pairs) {
    return hydra.sorting.Sorting.<T1>topologicalSortComponents(pairs);
  }
}
