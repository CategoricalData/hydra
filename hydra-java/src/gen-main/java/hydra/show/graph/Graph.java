// Note: this is an automatically generated file. Do not edit.

package hydra.show.graph;

/**
 * String representations of hydra.graph types
 */
public interface Graph {
  static String graph(hydra.graph.Graph graph) {
    java.util.List<hydra.core.Binding> elements = hydra.lib.maps.Elems.apply(((graph)).elements);
    java.util.List<String> elementStrs = hydra.lib.lists.Map.apply(
      (hydra.show.core.Core::binding),
      (elements));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "{",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        (elementStrs)),
      "}"));
  }
}
