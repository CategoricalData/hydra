// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class InsertEdgeAndNode implements Serializable, Comparable<InsertEdgeAndNode> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.InsertEdgeAndNode");

  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");

  public static final hydra.core.Name NODE = new hydra.core.Name("node");

  public final openGql.grammar.InsertEdgePattern edge;

  public final hydra.util.Maybe<openGql.grammar.InsertElementPatternFiller> node;

  public InsertEdgeAndNode (openGql.grammar.InsertEdgePattern edge, hydra.util.Maybe<openGql.grammar.InsertElementPatternFiller> node) {
    this.edge = edge;
    this.node = node;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InsertEdgeAndNode)) {
      return false;
    }
    InsertEdgeAndNode o = (InsertEdgeAndNode) other;
    return java.util.Objects.equals(
      this.edge,
      o.edge) && java.util.Objects.equals(
      this.node,
      o.node);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(edge) + 3 * java.util.Objects.hashCode(node);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InsertEdgeAndNode other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      edge,
      other.edge);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      node,
      other.node);
  }

  public InsertEdgeAndNode withEdge(openGql.grammar.InsertEdgePattern edge) {
    return new InsertEdgeAndNode(edge, node);
  }

  public InsertEdgeAndNode withNode(hydra.util.Maybe<openGql.grammar.InsertElementPatternFiller> node) {
    return new InsertEdgeAndNode(edge, node);
  }
}
