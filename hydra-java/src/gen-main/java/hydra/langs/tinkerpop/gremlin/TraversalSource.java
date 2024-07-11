// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalSource implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalSource");
  
  public final java.util.List<hydra.langs.tinkerpop.gremlin.TraversalSourceSelfMethod> value;
  
  public TraversalSource (java.util.List<hydra.langs.tinkerpop.gremlin.TraversalSourceSelfMethod> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalSource)) {
      return false;
    }
    TraversalSource o = (TraversalSource) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}