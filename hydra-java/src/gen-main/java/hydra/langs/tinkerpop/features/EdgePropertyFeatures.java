// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Edge Property objects.
 */
public class EdgePropertyFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/features.EdgePropertyFeatures");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY_FEATURES = new hydra.core.Name("propertyFeatures");
  
  public final hydra.langs.tinkerpop.features.PropertyFeatures propertyFeatures;
  
  public EdgePropertyFeatures (hydra.langs.tinkerpop.features.PropertyFeatures propertyFeatures) {
    java.util.Objects.requireNonNull((propertyFeatures));
    this.propertyFeatures = propertyFeatures;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgePropertyFeatures)) {
      return false;
    }
    EdgePropertyFeatures o = (EdgePropertyFeatures) (other);
    return propertyFeatures.equals(o.propertyFeatures);
  }
  
  @Override
  public int hashCode() {
    return 2 * propertyFeatures.hashCode();
  }
}