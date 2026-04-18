// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class EdgeStmt implements Serializable, Comparable<EdgeStmt> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.EdgeStmt");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public static final hydra.core.Name ATTRIBUTES = new hydra.core.Name("attributes");

  public final hydra.graphviz.dot.NodeOrSubgraph left;

  public final java.util.List<hydra.graphviz.dot.NodeOrSubgraph> right;

  public final hydra.util.Maybe<hydra.graphviz.dot.AttrList> attributes;

  public EdgeStmt (hydra.graphviz.dot.NodeOrSubgraph left, java.util.List<hydra.graphviz.dot.NodeOrSubgraph> right, hydra.util.Maybe<hydra.graphviz.dot.AttrList> attributes) {
    this.left = left;
    this.right = right;
    this.attributes = attributes;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeStmt)) {
      return false;
    }
    EdgeStmt o = (EdgeStmt) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right) && java.util.Objects.equals(
      this.attributes,
      o.attributes);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right) + 5 * java.util.Objects.hashCode(attributes);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgeStmt other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      left,
      other.left);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      right,
      other.right);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      attributes,
      other.attributes);
  }

  public EdgeStmt withLeft(hydra.graphviz.dot.NodeOrSubgraph left) {
    return new EdgeStmt(left, right, attributes);
  }

  public EdgeStmt withRight(java.util.List<hydra.graphviz.dot.NodeOrSubgraph> right) {
    return new EdgeStmt(left, right, attributes);
  }

  public EdgeStmt withAttributes(hydra.util.Maybe<hydra.graphviz.dot.AttrList> attributes) {
    return new EdgeStmt(left, right, attributes);
  }
}
