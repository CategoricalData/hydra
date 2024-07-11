// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TwoTraversalPredicates implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TwoTraversalPredicates");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalPredicate left;
  
  public final hydra.langs.tinkerpop.gremlin.TraversalPredicate right;
  
  public TwoTraversalPredicates (hydra.langs.tinkerpop.gremlin.TraversalPredicate left, hydra.langs.tinkerpop.gremlin.TraversalPredicate right) {
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
  
  public TwoTraversalPredicates withLeft(hydra.langs.tinkerpop.gremlin.TraversalPredicate left) {
    java.util.Objects.requireNonNull((left));
    return new TwoTraversalPredicates(left, right);
  }
  
  public TwoTraversalPredicates withRight(hydra.langs.tinkerpop.gremlin.TraversalPredicate right) {
    java.util.Objects.requireNonNull((right));
    return new TwoTraversalPredicates(left, right);
  }
}