package hydra.ext.tinkerpop.features;

/**
 * Features that are related to Edge Property objects.
 */
public class EdgePropertyFeatures {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/tinkerpop/features.EdgePropertyFeatures");
  
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