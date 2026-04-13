// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class TwoTraversalPredicates implements Serializable, Comparable<TwoTraversalPredicates> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.TwoTraversalPredicates");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final hydra.tinkerpop.gremlin.TraversalPredicate left;

  public final hydra.tinkerpop.gremlin.TraversalPredicate right;

  public TwoTraversalPredicates (hydra.tinkerpop.gremlin.TraversalPredicate left, hydra.tinkerpop.gremlin.TraversalPredicate right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TwoTraversalPredicates)) {
      return false;
    }
    TwoTraversalPredicates o = (TwoTraversalPredicates) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TwoTraversalPredicates other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      left,
      other.left);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      right,
      other.right);
  }

  public TwoTraversalPredicates withLeft(hydra.tinkerpop.gremlin.TraversalPredicate left) {
    return new TwoTraversalPredicates(left, right);
  }

  public TwoTraversalPredicates withRight(hydra.tinkerpop.gremlin.TraversalPredicate right) {
    return new TwoTraversalPredicates(left, right);
  }
}
