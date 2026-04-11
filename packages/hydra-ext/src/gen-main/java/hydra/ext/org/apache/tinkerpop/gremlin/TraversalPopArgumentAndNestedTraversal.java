// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalPopArgumentAndNestedTraversal implements Serializable, Comparable<TraversalPopArgumentAndNestedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal");

  public static final hydra.core.Name POP = new hydra.core.Name("pop");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument pop;

  public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal;

  public TraversalPopArgumentAndNestedTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument pop, hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal) {
    this.pop = pop;
    this.traversal = traversal;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalPopArgumentAndNestedTraversal)) {
      return false;
    }
    TraversalPopArgumentAndNestedTraversal o = (TraversalPopArgumentAndNestedTraversal) other;
    return java.util.Objects.equals(
      this.pop,
      o.pop) && java.util.Objects.equals(
      this.traversal,
      o.traversal);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pop) + 3 * java.util.Objects.hashCode(traversal);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TraversalPopArgumentAndNestedTraversal other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pop,
      other.pop);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      traversal,
      other.traversal);
  }

  public TraversalPopArgumentAndNestedTraversal withPop(hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument pop) {
    return new TraversalPopArgumentAndNestedTraversal(pop, traversal);
  }

  public TraversalPopArgumentAndNestedTraversal withTraversal(hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal) {
    return new TraversalPopArgumentAndNestedTraversal(pop, traversal);
  }
}
