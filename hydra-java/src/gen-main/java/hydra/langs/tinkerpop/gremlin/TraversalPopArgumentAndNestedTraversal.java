// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalPopArgumentAndNestedTraversal implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalPopArgumentAndNestedTraversal");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalPopArgument pop;
  
  public final hydra.langs.tinkerpop.gremlin.NestedTraversal traversal;
  
  public TraversalPopArgumentAndNestedTraversal (hydra.langs.tinkerpop.gremlin.TraversalPopArgument pop, hydra.langs.tinkerpop.gremlin.NestedTraversal traversal) {
    if (pop == null) {
      throw new IllegalArgumentException("null value for 'pop' argument");
    }
    if (traversal == null) {
      throw new IllegalArgumentException("null value for 'traversal' argument");
    }
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
  
  public TraversalPopArgumentAndNestedTraversal withPop(hydra.langs.tinkerpop.gremlin.TraversalPopArgument pop) {
    if (pop == null) {
      throw new IllegalArgumentException("null value for 'pop' argument");
    }
    return new TraversalPopArgumentAndNestedTraversal(pop, traversal);
  }
  
  public TraversalPopArgumentAndNestedTraversal withTraversal(hydra.langs.tinkerpop.gremlin.NestedTraversal traversal) {
    if (traversal == null) {
      throw new IllegalArgumentException("null value for 'traversal' argument");
    }
    return new TraversalPopArgumentAndNestedTraversal(pop, traversal);
  }
}