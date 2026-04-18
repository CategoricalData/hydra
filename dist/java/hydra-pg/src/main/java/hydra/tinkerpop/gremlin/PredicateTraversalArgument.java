// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class PredicateTraversalArgument implements Serializable, Comparable<PredicateTraversalArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.PredicateTraversalArgument");

  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");

  public static final hydra.core.Name TRAVERSAL1 = new hydra.core.Name("traversal1");

  public static final hydra.core.Name TRAVERSAL2 = new hydra.core.Name("traversal2");

  public final hydra.tinkerpop.gremlin.TraversalPredicate predicate;

  public final hydra.tinkerpop.gremlin.NestedTraversal traversal1;

  public final hydra.util.Maybe<hydra.tinkerpop.gremlin.NestedTraversal> traversal2;

  public PredicateTraversalArgument (hydra.tinkerpop.gremlin.TraversalPredicate predicate, hydra.tinkerpop.gremlin.NestedTraversal traversal1, hydra.util.Maybe<hydra.tinkerpop.gremlin.NestedTraversal> traversal2) {
    this.predicate = predicate;
    this.traversal1 = traversal1;
    this.traversal2 = traversal2;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PredicateTraversalArgument)) {
      return false;
    }
    PredicateTraversalArgument o = (PredicateTraversalArgument) other;
    return java.util.Objects.equals(
      this.predicate,
      o.predicate) && java.util.Objects.equals(
      this.traversal1,
      o.traversal1) && java.util.Objects.equals(
      this.traversal2,
      o.traversal2);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(predicate) + 3 * java.util.Objects.hashCode(traversal1) + 5 * java.util.Objects.hashCode(traversal2);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PredicateTraversalArgument other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      predicate,
      other.predicate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      traversal1,
      other.traversal1);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      traversal2,
      other.traversal2);
  }

  public PredicateTraversalArgument withPredicate(hydra.tinkerpop.gremlin.TraversalPredicate predicate) {
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }

  public PredicateTraversalArgument withTraversal1(hydra.tinkerpop.gremlin.NestedTraversal traversal1) {
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }

  public PredicateTraversalArgument withTraversal2(hydra.util.Maybe<hydra.tinkerpop.gremlin.NestedTraversal> traversal2) {
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }
}
