package hydra.langs.tinkerpop.features;

/**
 * Features that are related to Edge Property objects.
 */
public class EdgePropertyFeatures {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/features.EdgePropertyFeatures");
  
  public final hydra.langs.tinkerpop.features.PropertyFeatures propertyFeatures;
  
  public EdgePropertyFeatures (hydra.langs.tinkerpop.features.PropertyFeatures propertyFeatures) {
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