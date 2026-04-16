// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class OpenGraphType implements Serializable, Comparable<OpenGraphType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OpenGraphType");

  public static final hydra.core.Name TYPED = new hydra.core.Name("typed");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public final hydra.util.Maybe<java.lang.Void> typed;

  public final Boolean graph;

  public OpenGraphType (hydra.util.Maybe<java.lang.Void> typed, Boolean graph) {
    this.typed = typed;
    this.graph = graph;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OpenGraphType)) {
      return false;
    }
    OpenGraphType o = (OpenGraphType) other;
    return java.util.Objects.equals(
      this.typed,
      o.typed) && java.util.Objects.equals(
      this.graph,
      o.graph);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typed) + 3 * java.util.Objects.hashCode(graph);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OpenGraphType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      typed,
      other.typed);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      graph,
      other.graph);
  }

  public OpenGraphType withTyped(hydra.util.Maybe<java.lang.Void> typed) {
    return new OpenGraphType(typed, graph);
  }

  public OpenGraphType withGraph(Boolean graph) {
    return new OpenGraphType(typed, graph);
  }
}
