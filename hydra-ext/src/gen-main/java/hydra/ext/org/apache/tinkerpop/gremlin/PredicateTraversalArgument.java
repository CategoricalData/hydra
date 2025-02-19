// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class PredicateTraversalArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.PredicateTraversalArgument");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE = new hydra.core.Name("predicate");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL1 = new hydra.core.Name("traversal1");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL2 = new hydra.core.Name("traversal2");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal1;
  
  public final hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> traversal2;
  
  public PredicateTraversalArgument (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate, hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal1, hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> traversal2) {
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
  
  public PredicateTraversalArgument withPredicate(hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate) {
    java.util.Objects.requireNonNull((predicate));
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }
  
  public PredicateTraversalArgument withTraversal1(hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal1) {
    java.util.Objects.requireNonNull((traversal1));
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }
  
  public PredicateTraversalArgument withTraversal2(hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> traversal2) {
    java.util.Objects.requireNonNull((traversal2));
    return new PredicateTraversalArgument(predicate, traversal1, traversal2);
  }
}