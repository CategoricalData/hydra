// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class FocusedLinearQueryStatementPart implements Serializable, Comparable<FocusedLinearQueryStatementPart> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FocusedLinearQueryStatementPart");

  public static final hydra.core.Name USE_GRAPH = new hydra.core.Name("useGraph");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  public final openGql.grammar.GraphExpression useGraph;

  public final java.util.List<openGql.grammar.SimpleQueryStatement> simple;

  public FocusedLinearQueryStatementPart (openGql.grammar.GraphExpression useGraph, java.util.List<openGql.grammar.SimpleQueryStatement> simple) {
    this.useGraph = useGraph;
    this.simple = simple;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FocusedLinearQueryStatementPart)) {
      return false;
    }
    FocusedLinearQueryStatementPart o = (FocusedLinearQueryStatementPart) other;
    return java.util.Objects.equals(
      this.useGraph,
      o.useGraph) && java.util.Objects.equals(
      this.simple,
      o.simple);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(useGraph) + 3 * java.util.Objects.hashCode(simple);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FocusedLinearQueryStatementPart other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      useGraph,
      other.useGraph);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      simple,
      other.simple);
  }

  public FocusedLinearQueryStatementPart withUseGraph(openGql.grammar.GraphExpression useGraph) {
    return new FocusedLinearQueryStatementPart(useGraph, simple);
  }

  public FocusedLinearQueryStatementPart withSimple(java.util.List<openGql.grammar.SimpleQueryStatement> simple) {
    return new FocusedLinearQueryStatementPart(useGraph, simple);
  }
}
