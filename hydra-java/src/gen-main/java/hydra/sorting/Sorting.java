// Note: this is an automatically generated file. Do not edit.

package hydra.sorting;

/**
 * Utilities for sorting.
 */
public interface Sorting {
  static <T0, T1> java.util.Map<T0, java.util.List<T1>> adjacencyListToMap(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>> pairs) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<T0, java.util.List<T1>>, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>, java.util.Map<T0, java.util.List<T1>>>>) (mp -> (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>, java.util.Map<T0, java.util.List<T1>>>) (p -> {
        hydra.util.Lazy<T0> k = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.<T0, java.util.List<T1>>adjacencyListToMap_k((p)));
        return hydra.lib.maps.Insert.apply(
          k.get(),
          hydra.lib.lists.Concat2.apply(
            hydra.sorting.Sorting.<T0, T1>adjacencyListToMap_existing(
              k.get(),
              (mp)),
            hydra.sorting.Sorting.<T0, java.util.List<T1>>adjacencyListToMap_vs((p))),
          (mp));
      })),
      (java.util.Map<T0, java.util.List<T1>>) ((java.util.Map<T0, java.util.List<T1>>) (hydra.lib.maps.Empty.<T0, java.util.List<T1>>apply())),
      (pairs));
  }
  
  static <T0, T1> T0 adjacencyListToMap_k(hydra.util.Tuple.Tuple2<T0, T1> p) {
    return hydra.lib.pairs.First.apply((p));
  }
  
  static <T0, T1> T1 adjacencyListToMap_vs(hydra.util.Tuple.Tuple2<T0, T1> p) {
    return hydra.lib.pairs.Second.apply((p));
  }
  
  static <T0, T1> java.util.List<T1> adjacencyListToMap_existing(T0 k, java.util.Map<T0, java.util.List<T1>> mp) {
    return hydra.lib.maybes.Maybe.apply(
      (java.util.List<T1>) (java.util.List.<T1>of()),
      (java.util.function.Function<java.util.List<T1>, java.util.List<T1>>) ((hydra.lib.equality.Identity::apply)),
      hydra.lib.maps.Lookup.apply(
        (k),
        (mp)));
  }
  
  static <T0, T1> hydra.topology.OrderingIsomorphism<T1> createOrderingIsomorphism(java.util.List<T0> sourceOrd, java.util.List<T0> targetOrd) {
    return (hydra.topology.OrderingIsomorphism<T1>) (new hydra.topology.OrderingIsomorphism<T1>((java.util.function.Function<java.util.List<T1>, java.util.List<T1>>) (v1 -> hydra.sorting.Sorting.<T0, T1>createOrderingIsomorphism_sourceToTargetMapping(
      (sourceOrd),
      (targetOrd),
      (v1))), (java.util.function.Function<java.util.List<T1>, java.util.List<T1>>) (v1 -> hydra.sorting.Sorting.<T0, T1>createOrderingIsomorphism_targetToSourceMapping(
      (sourceOrd),
      (targetOrd),
      (v1)))));
  }
  
  static <T0, T1> java.util.List<T1> createOrderingIsomorphism_sourceToTargetMapping(java.util.List<T0> sourceOrd, java.util.List<T0> targetOrd, java.util.List<T1> els) {
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Maybe<T1>>) (n -> hydra.lib.maps.Lookup.apply(
        (n),
        hydra.sorting.Sorting.<T1, T0>createOrderingIsomorphism_mp2(
          (els),
          (sourceOrd)))),
      (targetOrd)));
  }
  
  static <T0, T1> java.util.List<T1> createOrderingIsomorphism_targetToSourceMapping(java.util.List<T0> sourceOrd, java.util.List<T0> targetOrd, java.util.List<T1> els) {
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Maybe<T1>>) (n -> hydra.lib.maps.Lookup.apply(
        (n),
        hydra.sorting.Sorting.<T1, T0>createOrderingIsomorphism_mp(
          (els),
          (targetOrd)))),
      (sourceOrd)));
  }
  
  static <T0, T1> java.util.Map<T1, T0> createOrderingIsomorphism_mp(java.util.List<T0> els, java.util.List<T1> targetOrd) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      (targetOrd),
      (els)));
  }
  
  static <T0, T1> java.util.Map<T1, T0> createOrderingIsomorphism_mp2(java.util.List<T0> els, java.util.List<T1> sourceOrd) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      (sourceOrd),
      (els)));
  }
  
  static <T0> java.util.Set<T0> findReachableNodes(java.util.function.Function<T0, java.util.Set<T0>> adj, T0 root) {
    return hydra.sorting.Sorting.<T0>findReachableNodes_visit(
      (adj),
      hydra.lib.sets.Singleton.apply((root)),
      (root));
  }
  
  static <T0> java.util.Set<T0> findReachableNodes_visit(java.util.function.Function<T0, java.util.Set<T0>> adj, java.util.Set<T0> visited, T0 node) {
    hydra.util.Lazy<java.util.Set<T0>> toVisit = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.<T0, T0>findReachableNodes_toVisit(
      (adj),
      (node),
      (visited)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(toVisit.get()),
      () -> (visited),
      () -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<java.util.Set<T0>, java.util.function.Function<T0, java.util.Set<T0>>>) (v -> (java.util.function.Function<T0, java.util.Set<T0>>) (n -> hydra.sorting.Sorting.<T0>findReachableNodes_visit(
          (adj),
          hydra.lib.sets.Insert.apply(
            (n),
            (v)),
          (n)))),
        (visited),
        hydra.lib.sets.ToList.apply(toVisit.get())));
  }
  
  static <T0, T1> java.util.Set<T1> findReachableNodes_toVisit(java.util.function.Function<T0, java.util.Set<T1>> adj, T0 node, java.util.Set<T1> visited) {
    return hydra.lib.sets.Difference.apply(
      ((adj)).apply((node)),
      (visited));
  }
  
  static <T0, T1> java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.Set<T1>>> propagateTags(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>> edges, java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>> nodeTags) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Tuple.Tuple2<T0, java.util.Set<T1>>>) (n -> (hydra.util.Tuple.Tuple2<T0, java.util.Set<T1>>) ((hydra.util.Tuple.Tuple2<T0, java.util.Set<T1>>) (new hydra.util.Tuple.Tuple2<T0, java.util.Set<T1>>((n), hydra.sorting.Sorting.<T0, T1>propagateTags_getTagsForNode(
        hydra.sorting.Sorting.<T0, T0>propagateTags_adjMap((edges)),
        hydra.sorting.Sorting.<T0, T1>propagateTags_tagMap((nodeTags)),
        (n)))))),
      hydra.sorting.Sorting.<T0, java.util.List<T0>, java.util.List<T1>>propagateTags_allNodes(
        (edges),
        (nodeTags)));
  }
  
  static <T0, T1> java.util.Map<T0, java.util.List<T1>> propagateTags_adjMap(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>> edges) {
    return hydra.sorting.Sorting.<T0, T1>adjacencyListToMap((edges));
  }
  
  static <T0, T1> java.util.Map<T0, java.util.Set<T1>> propagateTags_tagMap(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T1>>> nodeTags) {
    return hydra.lib.maps.Map.apply(
      (java.util.function.Function<java.util.List<T1>, java.util.Set<T1>>) ((hydra.lib.sets.FromList::apply)),
      hydra.sorting.Sorting.<T0, T1>adjacencyListToMap((nodeTags)));
  }
  
  static <T0, T1, T2> java.util.List<T0> propagateTags_allNodes(java.util.List<hydra.util.Tuple.Tuple2<T0, T1>> edges, java.util.List<hydra.util.Tuple.Tuple2<T0, T2>> nodeTags) {
    return hydra.lib.sets.ToList.apply(hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat2.apply(
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, T0>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, T0>) ((hydra.lib.pairs.First::apply))),
        (edges)),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T2>, T0>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T2>, T0>) ((hydra.lib.pairs.First::apply))),
        (nodeTags)))));
  }
  
  static <T0, T1> java.util.Set<T1> propagateTags_getTagsForNode(java.util.Map<T0, java.util.List<T0>> adjMap, java.util.Map<T0, java.util.Set<T1>> tagMap, T0 node) {
    return hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, java.util.Set<T1>>) (n -> hydra.lib.maybes.Maybe.apply(
        (java.util.Set<T1>) (hydra.lib.sets.Empty.<T1>apply()),
        (java.util.function.Function<java.util.Set<T1>, java.util.Set<T1>>) ((hydra.lib.equality.Identity::apply)),
        hydra.lib.maps.Lookup.apply(
          (n),
          (tagMap)))),
      hydra.lib.sets.ToList.apply(hydra.sorting.Sorting.<T0>propagateTags_reachable(
        (adjMap),
        (node)))));
  }
  
  static <T0> java.util.Set<T0> propagateTags_reachable(java.util.Map<T0, java.util.List<T0>> adjMap, T0 node) {
    return hydra.sorting.Sorting.<T0>findReachableNodes(
      (java.util.function.Function<T0, java.util.Set<T0>>) (n -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Maybe.apply(
        (java.util.List<T0>) (java.util.List.<T0>of()),
        (java.util.function.Function<java.util.List<T0>, java.util.List<T0>>) ((hydra.lib.equality.Identity::apply)),
        hydra.lib.maps.Lookup.apply(
          (n),
          (adjMap))))),
      (node));
  }
  
  static <T0> hydra.util.Either<java.util.List<java.util.List<T0>>, java.util.List<T0>> topologicalSort(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>> pairs) {
    hydra.util.Lazy<java.util.List<java.util.List<T0>>> sccs = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.<T0>topologicalSort_sccs((pairs)));
    hydra.util.Lazy<java.util.List<java.util.List<T0>>> withCycles = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.<T0>topologicalSort_withCycles(sccs.get()));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(withCycles.get()),
      () -> (hydra.util.Either<java.util.List<java.util.List<T0>>, java.util.List<T0>>) ((hydra.util.Either<java.util.List<java.util.List<T0>>, java.util.List<T0>>) (hydra.util.Either.<java.util.List<java.util.List<T0>>, java.util.List<T0>>right(hydra.lib.lists.Concat.apply(sccs.get())))),
      () -> (hydra.util.Either<java.util.List<java.util.List<T0>>, java.util.List<T0>>) ((hydra.util.Either<java.util.List<java.util.List<T0>>, java.util.List<T0>>) (hydra.util.Either.<java.util.List<java.util.List<T0>>, java.util.List<T0>>left(withCycles.get()))));
  }
  
  static <T0> java.util.List<java.util.List<T0>> topologicalSort_sccs(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>> pairs) {
    return hydra.sorting.Sorting.<T0>topologicalSortComponents((pairs));
  }
  
  static <T0> Boolean topologicalSort_isCycle(java.util.List<T0> scc) {
    return hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply((scc))));
  }
  
  static <T0> java.util.List<java.util.List<T0>> topologicalSort_withCycles(java.util.List<java.util.List<T0>> sccs) {
    return hydra.lib.lists.Filter.apply(
      p0 -> hydra.sorting.Sorting.<T0>topologicalSort_isCycle((p0)),
      (sccs));
  }
  
  static <T0> java.util.List<java.util.List<T0>> topologicalSortComponents(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>> pairs) {
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>>> graphResult = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.<T0>topologicalSortComponents_graphResult((pairs)));
    hydra.util.Lazy<java.util.Map<Integer, java.util.List<Integer>>> g = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(graphResult.get()));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<Integer>, java.util.List<T0>>) (comp -> hydra.lib.lists.Map.apply(
        hydra.lib.pairs.Second.apply(graphResult.get()),
        (comp))),
      hydra.tarjan.Tarjan.stronglyConnectedComponents(g.get()));
  }
  
  static <T0> hydra.util.Tuple.Tuple2<java.util.Map<Integer, java.util.List<Integer>>, java.util.function.Function<Integer, T0>> topologicalSortComponents_graphResult(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>> pairs) {
    return hydra.tarjan.Tarjan.<T0>adjacencyListsToGraph((pairs));
  }
  
  static <T0, T1> java.util.List<java.util.List<T0>> topologicalSortNodes(java.util.function.Function<T0, T1> getKey, java.util.function.Function<T0, java.util.List<T1>> getAdj, java.util.List<T0> nodes) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<T1>, java.util.List<T0>>) (c -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<T1, hydra.util.Maybe<T0>>) (k -> hydra.lib.maps.Lookup.apply(
          (k),
          hydra.sorting.Sorting.<T0, T1>topologicalSortNodes_nodesByKey(
            (getKey),
            (nodes)))),
        (c)))),
      hydra.sorting.Sorting.<T1>topologicalSortNodes_comps(hydra.sorting.Sorting.<T0, java.util.List<T1>, T1>topologicalSortNodes_pairs(
        (getAdj),
        (getKey),
        (nodes))));
  }
  
  static <T0, T1> java.util.Map<T1, T0> topologicalSortNodes_nodesByKey(java.util.function.Function<T0, T1> getKey, java.util.List<T0> nodes) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Tuple.Tuple2<T1, T0>>) (n -> (hydra.util.Tuple.Tuple2<T1, T0>) ((hydra.util.Tuple.Tuple2<T1, T0>) (new hydra.util.Tuple.Tuple2<T1, T0>(((getKey)).apply((n)), (n))))),
      (nodes)));
  }
  
  static <T0, T1, T2> java.util.List<hydra.util.Tuple.Tuple2<T2, T1>> topologicalSortNodes_pairs(java.util.function.Function<T0, T1> getAdj, java.util.function.Function<T0, T2> getKey, java.util.List<T0> nodes) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<T0, hydra.util.Tuple.Tuple2<T2, T1>>) (n -> (hydra.util.Tuple.Tuple2<T2, T1>) ((hydra.util.Tuple.Tuple2<T2, T1>) (new hydra.util.Tuple.Tuple2<T2, T1>(((getKey)).apply((n)), ((getAdj)).apply((n)))))),
      (nodes));
  }
  
  static <T0> java.util.List<java.util.List<T0>> topologicalSortNodes_comps(java.util.List<hydra.util.Tuple.Tuple2<T0, java.util.List<T0>>> pairs) {
    return hydra.sorting.Sorting.<T0>topologicalSortComponents((pairs));
  }
}
