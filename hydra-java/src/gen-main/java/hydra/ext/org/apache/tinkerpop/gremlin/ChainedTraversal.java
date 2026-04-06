// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class ChainedTraversal implements Serializable, Comparable<ChainedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversal");

  public static final hydra.core.Name FIRST = new hydra.core.Name("first");

  public static final hydra.core.Name REST = new hydra.core.Name("rest");

  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod first;

  public final hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement rest;

  public ChainedTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod first, hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement rest) {
    this.first = first;
    this.rest = rest;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ChainedTraversal)) {
      return false;
    }
    ChainedTraversal o = (ChainedTraversal) other;
    return java.util.Objects.equals(
      this.first,
      o.first) && java.util.Objects.equals(
      this.rest,
      o.rest);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(first) + 3 * java.util.Objects.hashCode(rest);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ChainedTraversal other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      first,
      other.first);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rest,
      other.rest);
  }

  public ChainedTraversal withFirst(hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod first) {
    return new ChainedTraversal(first, rest);
  }

  public ChainedTraversal withRest(hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement rest) {
    return new ChainedTraversal(first, rest);
  }
}
