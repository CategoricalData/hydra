// Note: this is an automatically generated file. Do not edit.

package hydra.pg.printing;

/**
 * Printing functions for property graph elements
 */
public interface Printing {
  static <T0> String printEdge(java.util.function.Function<T0, String> printValue, hydra.pg.model.Edge<T0> edge) {
    hydra.util.Lazy<String> id = new hydra.util.Lazy<>(() -> (printValue).apply(((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.id)).apply(edge)));
    hydra.util.Lazy<String> inId = new hydra.util.Lazy<>(() -> (printValue).apply(((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.in)).apply(edge)));
    hydra.util.Lazy<String> label = new hydra.util.Lazy<>(() -> (((java.util.function.Function<hydra.pg.model.Edge<T0>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(edge)).value);
    hydra.util.Lazy<String> outId = new hydra.util.Lazy<>(() -> (printValue).apply(((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.out)).apply(edge)));
    hydra.util.Lazy<String> props = new hydra.util.Lazy<>(() -> hydra.lib.strings.Intercalate.apply(
      ", ",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.pg.model.PropertyKey, T0>, String>) (p -> hydra.pg.printing.Printing.<T0>printProperty(
          printValue,
          hydra.lib.pairs.First.apply(p),
          hydra.lib.pairs.Second.apply(p))),
        hydra.lib.maps.ToList.apply(((java.util.function.Function<hydra.pg.model.Edge<T0>, hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>>) (projected -> projected.properties)).apply(edge)))));
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      id.get(),
      ": ",
      "(",
      outId.get(),
      ")-[:",
      label.get(),
      " {",
      props.get(),
      "}]->(",
      inId.get(),
      ")"));
  }
  
  static <T0> String printGraph(java.util.function.Function<T0, String> printValue, hydra.pg.model.Graph<T0> graph) {
    return hydra.pg.printing.Printing.<T0>printLazyGraph(
      printValue,
      (hydra.pg.model.LazyGraph<T0>) (new hydra.pg.model.LazyGraph<T0>(hydra.lib.maps.Elems.apply(((java.util.function.Function<hydra.pg.model.Graph<T0>, hydra.util.PersistentMap<T0, hydra.pg.model.Vertex<T0>>>) (projected -> projected.vertices)).apply(graph)), hydra.lib.maps.Elems.apply(((java.util.function.Function<hydra.pg.model.Graph<T0>, hydra.util.PersistentMap<T0, hydra.pg.model.Edge<T0>>>) (projected -> projected.edges)).apply(graph)))));
  }
  
  static <T0> String printLazyGraph(java.util.function.Function<T0, String> printValue, hydra.pg.model.LazyGraph<T0> lg) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "vertices:",
      hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.Vertex<T0>, String>) (v -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "\n\t",
          hydra.pg.printing.Printing.<T0>printVertex(
            printValue,
            v)))),
        hydra.pg.printing.Printing.<T0>printLazyGraph_vertices(lg))),
      "\nedges:",
      hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.Edge<T0>, String>) (e -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "\n\t",
          hydra.pg.printing.Printing.<T0>printEdge(
            printValue,
            e)))),
        hydra.pg.printing.Printing.<T0>printLazyGraph_edges(lg)))));
  }
  
  static <T0> hydra.util.ConsList<hydra.pg.model.Vertex<T0>> printLazyGraph_vertices(hydra.pg.model.LazyGraph<T0> lg) {
    return ((java.util.function.Function<hydra.pg.model.LazyGraph<T0>, hydra.util.ConsList<hydra.pg.model.Vertex<T0>>>) (projected -> projected.vertices)).apply(lg);
  }
  
  static <T0> hydra.util.ConsList<hydra.pg.model.Edge<T0>> printLazyGraph_edges(hydra.pg.model.LazyGraph<T0> lg) {
    return ((java.util.function.Function<hydra.pg.model.LazyGraph<T0>, hydra.util.ConsList<hydra.pg.model.Edge<T0>>>) (projected -> projected.edges)).apply(lg);
  }
  
  static <T0> String printProperty(java.util.function.Function<T0, String> printValue, hydra.pg.model.PropertyKey key, T0 value) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      (key).value,
      ": ",
      (printValue).apply(value)));
  }
  
  static <T0> String printVertex(java.util.function.Function<T0, String> printValue, hydra.pg.model.Vertex<T0> vertex) {
    hydra.util.Lazy<String> id = new hydra.util.Lazy<>(() -> (printValue).apply(((java.util.function.Function<hydra.pg.model.Vertex<T0>, T0>) (projected -> projected.id)).apply(vertex)));
    hydra.util.Lazy<String> label = new hydra.util.Lazy<>(() -> (((java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(vertex)).value);
    hydra.util.Lazy<String> props = new hydra.util.Lazy<>(() -> hydra.lib.strings.Intercalate.apply(
      ", ",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.pg.model.PropertyKey, T0>, String>) (p -> hydra.pg.printing.Printing.<T0>printProperty(
          printValue,
          hydra.lib.pairs.First.apply(p),
          hydra.lib.pairs.Second.apply(p))),
        hydra.lib.maps.ToList.apply(((java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>>) (projected -> projected.properties)).apply(vertex)))));
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      id.get(),
      ": (",
      label.get(),
      ": {",
      props.get(),
      "})"));
  }
}
