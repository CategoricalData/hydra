// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalSource implements Serializable, Comparable<TraversalSource> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSource");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod> value;
  
  public TraversalSource (hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalSource)) {
      return false;
    }
    TraversalSource o = (TraversalSource) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TraversalSource other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
