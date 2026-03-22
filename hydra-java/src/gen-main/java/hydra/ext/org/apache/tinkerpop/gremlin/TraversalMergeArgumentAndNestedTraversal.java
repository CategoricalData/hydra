// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalMergeArgumentAndNestedTraversal implements Serializable, Comparable<TraversalMergeArgumentAndNestedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal");

  public static final hydra.core.Name MERGE = new hydra.core.Name("merge");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgument merge;

  public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal;

  public TraversalMergeArgumentAndNestedTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgument merge, hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal) {
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
    cmp = ((Comparable) merge).compareTo(other.merge);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) traversal).compareTo(other.traversal);
  }

  public TraversalMergeArgumentAndNestedTraversal withMerge(hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgument merge) {
    return new TraversalMergeArgumentAndNestedTraversal(merge, traversal);
  }

  public TraversalMergeArgumentAndNestedTraversal withTraversal(hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal) {
    return new TraversalMergeArgumentAndNestedTraversal(merge, traversal);
  }
}
