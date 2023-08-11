package hydra.langs.tinkerpop.propertyGraph;

/**
 * The type of a property
 */
public class PropertyType<P> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.PropertyType");
  
  public final hydra.langs.tinkerpop.propertyGraph.PropertyKey key;
  
  public final P value;
  
  public PropertyType (hydra.langs.tinkerpop.propertyGraph.PropertyKey key, P value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyType)) {
      return false;
    }
    PropertyType o = (PropertyType) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public PropertyType withKey(hydra.langs.tinkerpop.propertyGraph.PropertyKey key) {
    return new PropertyType(key, value);
  }
  
  public PropertyType withValue(P value) {
    return new PropertyType(key, value);
  }
}