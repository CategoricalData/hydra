// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class TwoTraversalPredicates implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TwoTraversalPredicates");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate left;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate right;
  
  public TwoTraversalPredicates (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate left, hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TwoTraversalPredicates)) {
      return false;
    }
    TwoTraversalPredicates o = (TwoTraversalPredicates) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public TwoTraversalPredicates withLeft(hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate left) {
    java.util.Objects.requireNonNull((left));
    return new TwoTraversalPredicates(left, right);
  }
  
  public TwoTraversalPredicates withRight(hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate right) {
    java.util.Objects.requireNonNull((right));
    return new TwoTraversalPredicates(left, right);
  }
}