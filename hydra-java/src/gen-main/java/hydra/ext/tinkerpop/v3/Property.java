package hydra.ext.tinkerpop.v3;

public class Property {
  public final hydra.ext.tinkerpop.v3.PropertyKey key;
  
  public final hydra.ext.tinkerpop.v3.Value value;
  
  /**
   * Constructs an immutable Property object
   */
  public Property(hydra.ext.tinkerpop.v3.PropertyKey key, hydra.ext.tinkerpop.v3.Value value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Property)) {
        return false;
    }
    Property o = (Property) other;
    return key.equals(o.key)
        && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode()
        + 3 * value.hashCode();
  }
  
  /**
   * Construct a new immutable Property object in which key is overridden
   */
  public Property withKey(hydra.ext.tinkerpop.v3.PropertyKey key) {
    return new Property(key, value);
  }
  
  /**
   * Construct a new immutable Property object in which value is overridden
   */
  public Property withValue(hydra.ext.tinkerpop.v3.Value value) {
    return new Property(key, value);
  }
}
