// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalPopArgumentAndNestedTraversal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal");
  
  public static final hydra.core.Name FIELD_NAME_POP = new hydra.core.Name("pop");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL = new hydra.core.Name("traversal");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument pop;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal;
  
  public TraversalPopArgumentAndNestedTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument pop, hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal) {
    java.util.Objects.requireNonNull((pop));
    java.util.Objects.requireNonNull((traversal));
    this.pop = pop;
    this.traversal = traversal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalPopArgumentAndNestedTraversal)) {
      return false;
    }
    TraversalPopArgumentAndNestedTraversal o = (TraversalPopArgumentAndNestedTraversal) (other);
    return pop.equals(o.pop) && traversal.equals(o.traversal);
  }
  
  @Override
  public int hashCode() {
    return 2 * pop.hashCode() + 3 * traversal.hashCode();
  }
  
  public TraversalPopArgumentAndNestedTraversal withPop(hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument pop) {
    java.util.Objects.requireNonNull((pop));
    return new TraversalPopArgumentAndNestedTraversal(pop, traversal);
  }
  
  public TraversalPopArgumentAndNestedTraversal withTraversal(hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal) {
    java.util.Objects.requireNonNull((traversal));
    return new TraversalPopArgumentAndNestedTraversal(pop, traversal);
  }
}