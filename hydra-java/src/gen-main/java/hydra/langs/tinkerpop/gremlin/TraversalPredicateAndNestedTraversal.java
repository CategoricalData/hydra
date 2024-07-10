// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalPredicateAndNestedTraversal implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalPredicateAndNestedTraversal");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate;
  
  public final hydra.langs.tinkerpop.gremlin.NestedTraversal traversal;
  
  public TraversalPredicateAndNestedTraversal (hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate, hydra.langs.tinkerpop.gremlin.NestedTraversal traversal) {
    if (predicate == null) {
      throw new IllegalArgumentException("null value for 'predicate' argument");
    }
    if (traversal == null) {
      throw new IllegalArgumentException("null value for 'traversal' argument");
    }
    this.predicate = predicate;
    this.traversal = traversal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalPredicateAndNestedTraversal)) {
      return false;
    }
    TraversalPredicateAndNestedTraversal o = (TraversalPredicateAndNestedTraversal) (other);
    return predicate.equals(o.predicate) && traversal.equals(o.traversal);
  }
  
  @Override
  public int hashCode() {
    return 2 * predicate.hashCode() + 3 * traversal.hashCode();
  }
  
  public TraversalPredicateAndNestedTraversal withPredicate(hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate) {
    if (predicate == null) {
      throw new IllegalArgumentException("null value for 'predicate' argument");
    }
    return new TraversalPredicateAndNestedTraversal(predicate, traversal);
  }
  
  public TraversalPredicateAndNestedTraversal withTraversal(hydra.langs.tinkerpop.gremlin.NestedTraversal traversal) {
    if (traversal == null) {
      throw new IllegalArgumentException("null value for 'traversal' argument");
    }
    return new TraversalPredicateAndNestedTraversal(predicate, traversal);
  }
}