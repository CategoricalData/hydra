// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Edge Property objects.
 */
public class EdgePropertyFeatures implements Serializable, Comparable<EdgePropertyFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.features.EdgePropertyFeatures");

  public static final hydra.core.Name PROPERTY_FEATURES = new hydra.core.Name("propertyFeatures");

  public final hydra.tinkerpop.features.PropertyFeatures propertyFeatures;

  public EdgePropertyFeatures (hydra.tinkerpop.features.PropertyFeatures propertyFeatures) {
    this.propertyFeatures = propertyFeatures;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgePropertyFeatures)) {
      return false;
    }
    EdgePropertyFeatures o = (EdgePropertyFeatures) other;
    return java.util.Objects.equals(
      this.propertyFeatures,
      o.propertyFeatures);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(propertyFeatures);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgePropertyFeatures other) {
    return hydra.util.Comparing.compare(
      propertyFeatures,
      other.propertyFeatures);
  }
}
