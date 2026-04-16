// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class Subgraph implements Serializable, Comparable<Subgraph> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.Subgraph");

  public static final hydra.core.Name SUBGRAPH_ID = new hydra.core.Name("subgraphId");

  public static final hydra.core.Name STATEMENTS = new hydra.core.Name("statements");

  public final hydra.util.Maybe<hydra.graphviz.dot.SubgraphId> subgraphId;

  public final java.util.List<hydra.graphviz.dot.Stmt> statements;

  public Subgraph (hydra.util.Maybe<hydra.graphviz.dot.SubgraphId> subgraphId, java.util.List<hydra.graphviz.dot.Stmt> statements) {
    this.subgraphId = subgraphId;
    this.statements = statements;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Subgraph)) {
      return false;
    }
    Subgraph o = (Subgraph) other;
    return java.util.Objects.equals(
      this.subgraphId,
      o.subgraphId) && java.util.Objects.equals(
      this.statements,
      o.statements);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(subgraphId) + 3 * java.util.Objects.hashCode(statements);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Subgraph other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      subgraphId,
      other.subgraphId);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      statements,
      other.statements);
  }

  public Subgraph withSubgraphId(hydra.util.Maybe<hydra.graphviz.dot.SubgraphId> subgraphId) {
    return new Subgraph(subgraphId, statements);
  }

  public Subgraph withStatements(java.util.List<hydra.graphviz.dot.Stmt> statements) {
    return new Subgraph(subgraphId, statements);
  }
}
