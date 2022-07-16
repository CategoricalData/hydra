package hydra.ext.pegasus.pdl;

public class PropertyKey {
  public final String value;
  
  public PropertyKey (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyKey)) {
      return false;
    }
    PropertyKey o = (PropertyKey) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}