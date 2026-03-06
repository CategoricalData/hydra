// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalPredicateAndNestedTraversal implements Serializable, Comparable<TraversalPredicateAndNestedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal");
  
  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");
  
  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal;
  
  public TraversalPredicateAndNestedTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate, hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal) {
    this.predicate = predicate;
    this.traversal = traversal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalPredicateAndNestedTraversal)) {
      return false;
    }
    TraversalPredicateAndNestedTraversal o = (TraversalPredicateAndNestedTraversal) other;
    return java.util.Objects.equals(
      this.predicate,
      o.predicate) && java.util.Objects.equals(
      this.traversal,
      o.traversal);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(predicate) + 3 * java.util.Objects.hashCode(traversal);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TraversalPredicateAndNestedTraversal other) {
    int cmp = 0;
    cmp = ((Comparable) predicate).compareTo(other.predicate);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) traversal).compareTo(other.traversal);
  }
  
  public TraversalPredicateAndNestedTraversal withPredicate(hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate) {
    return new TraversalPredicateAndNestedTraversal(predicate, traversal);
  }
  
  public TraversalPredicateAndNestedTraversal withTraversal(hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal) {
    return new TraversalPredicateAndNestedTraversal(predicate, traversal);
  }
}
