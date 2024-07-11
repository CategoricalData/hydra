// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class PredicateTraversalArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.PredicateTraversalArgument");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate;
  
  public final hydra.langs.tinkerpop.gremlin.NestedTraversal traversal1;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.NestedTraversal> traversal2;
  
  public PredicateTraversalArgument (hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate, hydra.langs.tinkerpop.gremlin.NestedTraversal traversal1, hydra.util.Opt<hydra.langs.tinkerpop.gremlin.NestedTraversal> traversal2) {
    java.util.Objects.requireNonNull((predicate));
    java.util.Objects.requireNonNull((traversal1));
    java.util.Objects.requireNonNull((traversal2));
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
    java.util.Objects.requireNonNull((predicate));
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }
  
  public PredicateTraversalArgument withTraversal1(hydra.langs.tinkerpop.gremlin.NestedTraversal traversal1) {
    java.util.Objects.requireNonNull((traversal1));
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }
  
  public PredicateTraversalArgument withTraversal2(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.NestedTraversal> traversal2) {
    java.util.Objects.requireNonNull((traversal2));
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }
}