// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class NodeStmt implements Serializable, Comparable<NodeStmt> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.NodeStmt");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name ATTRIBUTES = new hydra.core.Name("attributes");

  public final hydra.graphviz.dot.NodeId id;

  public final hydra.util.Maybe<hydra.graphviz.dot.AttrList> attributes;

  public NodeStmt (hydra.graphviz.dot.NodeId id, hydra.util.Maybe<hydra.graphviz.dot.AttrList> attributes) {
    this.id = id;
    this.attributes = attributes;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeStmt)) {
      return false;
    }
    NodeStmt o = (NodeStmt) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.attributes,
      o.attributes);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(attributes);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeStmt other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      attributes,
      other.attributes);
  }

  public NodeStmt withId(hydra.graphviz.dot.NodeId id) {
    return new NodeStmt(id, attributes);
  }

  public NodeStmt withAttributes(hydra.util.Maybe<hydra.graphviz.dot.AttrList> attributes) {
    return new NodeStmt(id, attributes);
  }
}
