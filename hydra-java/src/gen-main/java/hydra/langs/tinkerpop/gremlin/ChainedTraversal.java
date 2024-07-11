// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class ChainedTraversal implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.ChainedTraversal");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalMethod first;
  
  public final hydra.langs.tinkerpop.gremlin.ChainedTraversalElement rest;
  
  public ChainedTraversal (hydra.langs.tinkerpop.gremlin.TraversalMethod first, hydra.langs.tinkerpop.gremlin.ChainedTraversalElement rest) {
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
  
  public ChainedTraversal withFirst(hydra.langs.tinkerpop.gremlin.TraversalMethod first) {
    java.util.Objects.requireNonNull((first));
    return new ChainedTraversal(first, rest);
  }
  
  public ChainedTraversal withRest(hydra.langs.tinkerpop.gremlin.ChainedTraversalElement rest) {
    java.util.Objects.requireNonNull((rest));
    return new ChainedTraversal(first, rest);
  }
}