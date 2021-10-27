package hydra.ext.tinkerpop.v3;

/**
 * @type string
 */
public class PropertyKey {
  public final String value;
  
  /**
   * Constructs an immutable PropertyKey object
   */
  public PropertyKey(String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyKey)) {
        return false;
    }
    PropertyKey o = (PropertyKey) other;
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
