// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class ChainedTraversal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/gremlin.ChainedTraversal");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_REST = new hydra.core.Name("rest");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod first;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement rest;
  
  public ChainedTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod first, hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement rest) {
    java.util.Objects.requireNonNull((first));
    java.util.Objects.requireNonNull((rest));
    this.first = first;
    this.rest = rest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ChainedTraversal)) {
      return false;
    }
    ChainedTraversal o = (ChainedTraversal) (other);
    return first.equals(o.first) && rest.equals(o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * first.hashCode() + 3 * rest.hashCode();
  }
  
  public ChainedTraversal withFirst(hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod first) {
    java.util.Objects.requireNonNull((first));
    return new ChainedTraversal(first, rest);
  }
  
  public ChainedTraversal withRest(hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement rest) {
    java.util.Objects.requireNonNull((rest));
    return new ChainedTraversal(first, rest);
  }
}