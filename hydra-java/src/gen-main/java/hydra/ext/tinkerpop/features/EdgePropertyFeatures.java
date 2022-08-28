package hydra.ext.tinkerpop.features;

/**
 * Features that are related to Edge Property objects.
 */
public class EdgePropertyFeatures {
  public final hydra.ext.tinkerpop.features.PropertyFeatures propertyFeatures;
  
  public EdgePropertyFeatures (hydra.ext.tinkerpop.features.PropertyFeatures propertyFeatures) {
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