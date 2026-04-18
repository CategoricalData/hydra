// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class Graph implements Serializable, Comparable<Graph> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.Graph");

  public static final hydra.core.Name STRICT = new hydra.core.Name("strict");

  public static final hydra.core.Name DIRECTED = new hydra.core.Name("directed");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name STATEMENTS = new hydra.core.Name("statements");

  public final Boolean strict;

  public final Boolean directed;

  public final hydra.util.Maybe<hydra.graphviz.dot.Id> id;

  public final java.util.List<hydra.graphviz.dot.Stmt> statements;

  public Graph (Boolean strict, Boolean directed, hydra.util.Maybe<hydra.graphviz.dot.Id> id, java.util.List<hydra.graphviz.dot.Stmt> statements) {
    this.strict = strict;
    this.directed = directed;
    this.id = id;
    this.statements = statements;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) other;
    return java.util.Objects.equals(
      this.strict,
      o.strict) && java.util.Objects.equals(
      this.directed,
      o.directed) && java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.statements,
      o.statements);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(strict) + 3 * java.util.Objects.hashCode(directed) + 5 * java.util.Objects.hashCode(id) + 7 * java.util.Objects.hashCode(statements);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Graph other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      strict,
      other.strict);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      directed,
      other.directed);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      statements,
      other.statements);
  }

  public Graph withStrict(Boolean strict) {
    return new Graph(strict, directed, id, statements);
  }

  public Graph withDirected(Boolean directed) {
    return new Graph(strict, directed, id, statements);
  }

  public Graph withId(hydra.util.Maybe<hydra.graphviz.dot.Id> id) {
    return new Graph(strict, directed, id, statements);
  }

  public Graph withStatements(java.util.List<hydra.graphviz.dot.Stmt> statements) {
    return new Graph(strict, directed, id, statements);
  }
}
