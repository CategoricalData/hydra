// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class PredicateTraversalArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.PredicateTraversalArgument");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate;
  
  public final hydra.langs.tinkerpop.gremlin.NestedTraversal traversal1;
  
  public final java.util.Optional<hydra.langs.tinkerpop.gremlin.NestedTraversal> traversal2;
  
  public PredicateTraversalArgument (hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate, hydra.langs.tinkerpop.gremlin.NestedTraversal traversal1, java.util.Optional<hydra.langs.tinkerpop.gremlin.NestedTraversal> traversal2) {
    if (predicate == null) {
      throw new IllegalArgumentException("null value for 'predicate' argument");
    }
    if (traversal1 == null) {
      throw new IllegalArgumentException("null value for 'traversal1' argument");
    }
    if (traversal2 == null) {
      throw new IllegalArgumentException("null value for 'traversal2' argument");
    }
    this.predicate = predicate;
    this.traversal1 = traversal1;
    this.traversal2 = traversal2;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PredicateTraversalArgument)) {
      return false;
    }
    PredicateTraversalArgument o = (PredicateTraversalArgument) (other);
    return predicate.equals(o.predicate) && traversal1.equals(o.traversal1) && traversal2.equals(o.traversal2);
  }
  
  @Override
  public int hashCode() {
    return 2 * predicate.hashCode() + 3 * traversal1.hashCode() + 5 * traversal2.hashCode();
  }
  
  public PredicateTraversalArgument withPredicate(hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate) {
    if (predicate == null) {
      throw new IllegalArgumentException("null value for 'predicate' argument");
    }
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }
  
  public PredicateTraversalArgument withTraversal1(hydra.langs.tinkerpop.gremlin.NestedTraversal traversal1) {
    if (traversal1 == null) {
      throw new IllegalArgumentException("null value for 'traversal1' argument");
    }
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }
  
  public PredicateTraversalArgument withTraversal2(java.util.Optional<hydra.langs.tinkerpop.gremlin.NestedTraversal> traversal2) {
    if (traversal2 == null) {
      throw new IllegalArgumentException("null value for 'traversal2' argument");
    }
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }
}