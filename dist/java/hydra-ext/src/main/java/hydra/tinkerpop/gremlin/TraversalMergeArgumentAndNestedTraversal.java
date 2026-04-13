// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalMergeArgumentAndNestedTraversal implements Serializable, Comparable<TraversalMergeArgumentAndNestedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal");

  public static final hydra.core.Name MERGE = new hydra.core.Name("merge");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  public final hydra.tinkerpop.gremlin.TraversalMergeArgument merge;

  public final hydra.tinkerpop.gremlin.NestedTraversal traversal;

  public TraversalMergeArgumentAndNestedTraversal (hydra.tinkerpop.gremlin.TraversalMergeArgument merge, hydra.tinkerpop.gremlin.NestedTraversal traversal) {
    this.merge = merge;
    this.traversal = traversal;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalMergeArgumentAndNestedTraversal)) {
      return false;
    }
    TraversalMergeArgumentAndNestedTraversal o = (TraversalMergeArgumentAndNestedTraversal) other;
    return java.util.Objects.equals(
      this.merge,
      o.merge) && java.util.Objects.equals(
      this.traversal,
      o.traversal);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(merge) + 3 * java.util.Objects.hashCode(traversal);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TraversalMergeArgumentAndNestedTraversal other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      merge,
      other.merge);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      traversal,
      other.traversal);
  }

  public TraversalMergeArgumentAndNestedTraversal withMerge(hydra.tinkerpop.gremlin.TraversalMergeArgument merge) {
    return new TraversalMergeArgumentAndNestedTraversal(merge, traversal);
  }

  public TraversalMergeArgumentAndNestedTraversal withTraversal(hydra.tinkerpop.gremlin.NestedTraversal traversal) {
    return new TraversalMergeArgumentAndNestedTraversal(merge, traversal);
  }
}
