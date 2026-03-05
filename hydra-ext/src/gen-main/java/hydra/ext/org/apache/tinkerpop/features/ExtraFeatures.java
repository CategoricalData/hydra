// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Additional features which are needed for the complete specification of language constraints in Hydra, above and beyond TinkerPop Graph.Features
 */
public class ExtraFeatures<A> implements Serializable, Comparable<ExtraFeatures<A>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.ExtraFeatures");
  
  public static final hydra.core.Name SUPPORTS_MAP_KEY = new hydra.core.Name("supportsMapKey");
  
  public final java.util.function.Function<hydra.core.Type, Boolean> supportsMapKey;
  
  public ExtraFeatures (java.util.function.Function<hydra.core.Type, Boolean> supportsMapKey) {
    this.supportsMapKey = supportsMapKey;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExtraFeatures)) {
      return false;
    }
    ExtraFeatures o = (ExtraFeatures) other;
    return java.util.Objects.equals(
      this.supportsMapKey,
      o.supportsMapKey);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(supportsMapKey);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExtraFeatures other) {
    return Integer.compare(
      supportsMapKey.hashCode(),
      other.supportsMapKey.hashCode());
  }
}
