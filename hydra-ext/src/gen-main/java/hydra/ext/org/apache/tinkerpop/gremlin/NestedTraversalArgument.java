// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class NestedTraversalArgument implements Serializable, Comparable<NestedTraversalArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversalArgument");

  public static final hydra.core.Name TRAVERSAL1 = new hydra.core.Name("traversal1");

  public static final hydra.core.Name TRAVERSAL2 = new hydra.core.Name("traversal2");

  public static final hydra.core.Name TRAVERSAL3 = new hydra.core.Name("traversal3");

  public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal1;

  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> traversal2;

  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> traversal3;

  public NestedTraversalArgument (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal1, hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> traversal2, hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> traversal3) {
    this.traversal1 = traversal1;
    this.traversal2 = traversal2;
    this.traversal3 = traversal3;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NestedTraversalArgument)) {
      return false;
    }
    NestedTraversalArgument o = (NestedTraversalArgument) other;
    return java.util.Objects.equals(
      this.traversal1,
      o.traversal1) && java.util.Objects.equals(
      this.traversal2,
      o.traversal2) && java.util.Objects.equals(
      this.traversal3,
      o.traversal3);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(traversal1) + 3 * java.util.Objects.hashCode(traversal2) + 5 * java.util.Objects.hashCode(traversal3);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NestedTraversalArgument other) {
    int cmp = 0;
    cmp = ((Comparable) traversal1).compareTo(other.traversal1);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) traversal2).compareTo(other.traversal2);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) traversal3).compareTo(other.traversal3);
  }

  public NestedTraversalArgument withTraversal1(hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal1) {
    return new NestedTraversalArgument(traversal1, traversal2, traversal3);
  }

  public NestedTraversalArgument withTraversal2(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> traversal2) {
    return new NestedTraversalArgument(traversal1, traversal2, traversal3);
  }

  public NestedTraversalArgument withTraversal3(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> traversal3) {
    return new NestedTraversalArgument(traversal1, traversal2, traversal3);
  }
}
