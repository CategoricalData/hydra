// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.features;

/**
 * Additional features which are needed for the complete specification of language constraints in Hydra, above and beyond TinkerPop Graph.Features
 */
public class ExtraFeatures<A> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/features.ExtraFeatures");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_MAP_KEY = new hydra.core.Name("supportsMapKey");
  
  public final java.util.function.Function<hydra.core.Type, Boolean> supportsMapKey;
  
  public ExtraFeatures (java.util.function.Function<hydra.core.Type, Boolean> supportsMapKey) {
    java.util.Objects.requireNonNull((supportsMapKey));
    this.supportsMapKey = supportsMapKey;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExtraFeatures)) {
      return false;
    }
    ExtraFeatures o = (ExtraFeatures) (other);
    return supportsMapKey.equals(o.supportsMapKey);
  }
  
  @Override
  public int hashCode() {
    return 2 * supportsMapKey.hashCode();
  }
}
