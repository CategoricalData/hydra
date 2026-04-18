// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SessionSetGraphParameterClause implements Serializable, Comparable<SessionSetGraphParameterClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SessionSetGraphParameterClause");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public static final hydra.core.Name INITIALIZER = new hydra.core.Name("initializer");

  public final openGql.grammar.SessionSetParameterName graph;

  public final openGql.grammar.OptTypedGraphInitializer initializer;

  public SessionSetGraphParameterClause (openGql.grammar.SessionSetParameterName graph, openGql.grammar.OptTypedGraphInitializer initializer) {
    this.graph = graph;
    this.initializer = initializer;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SessionSetGraphParameterClause)) {
      return false;
    }
    SessionSetGraphParameterClause o = (SessionSetGraphParameterClause) other;
    return java.util.Objects.equals(
      this.graph,
      o.graph) && java.util.Objects.equals(
      this.initializer,
      o.initializer);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(graph) + 3 * java.util.Objects.hashCode(initializer);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SessionSetGraphParameterClause other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      graph,
      other.graph);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      initializer,
      other.initializer);
  }

  public SessionSetGraphParameterClause withGraph(openGql.grammar.SessionSetParameterName graph) {
    return new SessionSetGraphParameterClause(graph, initializer);
  }

  public SessionSetGraphParameterClause withInitializer(openGql.grammar.OptTypedGraphInitializer initializer) {
    return new SessionSetGraphParameterClause(graph, initializer);
  }
}
