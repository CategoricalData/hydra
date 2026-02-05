// Note: this is an automatically generated file. Do not edit.

package hydra.show.graph;

/**
 * String representations of hydra.graph types
 */
public interface Graph {
  static String graph(hydra.graph.Graph graph) {
    java.util.List<hydra.core.Binding> elements = ((graph)).elements;
    hydra.util.Lazy<java.util.List<String>> elementStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (hydra.show.core.Core::binding),
      (elements)));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "{",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        elementStrs.get()),
      "}"));
  }
}
