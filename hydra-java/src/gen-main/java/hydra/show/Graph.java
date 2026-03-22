// Note: this is an automatically generated file. Do not edit.

package hydra.show;

/**
 * String representations of hydra.graph types
 */
public interface Graph {
  static String graph(hydra.util.ConsList<hydra.core.Binding> elements) {
    hydra.util.Lazy<hydra.util.ConsList<String>> elementStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.show.Core::binding,
      elements));
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "{",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        elementStrs.get()),
      "}"));
  }
}
