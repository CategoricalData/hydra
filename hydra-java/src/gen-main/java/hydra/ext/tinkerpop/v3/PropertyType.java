package hydra.ext.tinkerpop.v3;

public class PropertyType {
  public final hydra.ext.tinkerpop.v3.PropertyKey key;
  
  public final hydra.ext.tinkerpop.v3.Type value;
  
  /**
   * Constructs an immutable PropertyType object
   */
  public PropertyType(hydra.ext.tinkerpop.v3.PropertyKey key, hydra.ext.tinkerpop.v3.Type value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyType)) {
        return false;
    }
    PropertyType o = (PropertyType) other;
    return key.equals(o.key)
        && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode()
        + 3 * value.hashCode();
  }
  
  /**
   * Construct a new immutable PropertyType object in which key is overridden
   */
  public PropertyType withKey(hydra.ext.tinkerpop.v3.PropertyKey key) {
    return new PropertyType(key, value);
  }
  
  /**
   * Construct a new immutable PropertyType object in which value is overridden
   */
  public PropertyType withValue(hydra.ext.tinkerpop.v3.Type value) {
    return new PropertyType(key, value);
  }
}
