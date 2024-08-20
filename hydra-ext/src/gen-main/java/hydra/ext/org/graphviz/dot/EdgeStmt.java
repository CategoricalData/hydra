// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public class EdgeStmt implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/graphviz/dot.EdgeStmt");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public final hydra.ext.org.graphviz.dot.NodeOrSubgraph left;
  
  public final hydra.ext.org.graphviz.dot.NodeOrSubgraph right;
  
  public final java.util.List<hydra.ext.org.graphviz.dot.EqualityPair> attributes;
  
  public EdgeStmt (hydra.ext.org.graphviz.dot.NodeOrSubgraph left, hydra.ext.org.graphviz.dot.NodeOrSubgraph right, java.util.List<hydra.ext.org.graphviz.dot.EqualityPair> attributes) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    java.util.Objects.requireNonNull((attributes));
    this.left = left;
    this.right = right;
    this.attributes = attributes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeStmt)) {
      return false;
    }
    EdgeStmt o = (EdgeStmt) (other);
    return left.equals(o.left) && right.equals(o.right) && attributes.equals(o.attributes);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode() + 5 * attributes.hashCode();
  }
  
  public EdgeStmt withLeft(hydra.ext.org.graphviz.dot.NodeOrSubgraph left) {
    java.util.Objects.requireNonNull((left));
    return new EdgeStmt(left, right, attributes);
  }
  
  public EdgeStmt withRight(hydra.ext.org.graphviz.dot.NodeOrSubgraph right) {
    java.util.Objects.requireNonNull((right));
    return new EdgeStmt(left, right, attributes);
  }
  
  public EdgeStmt withAttributes(java.util.List<hydra.ext.org.graphviz.dot.EqualityPair> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new EdgeStmt(left, right, attributes);
  }
}