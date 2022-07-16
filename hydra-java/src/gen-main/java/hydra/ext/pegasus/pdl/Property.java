package hydra.ext.pegasus.pdl;

public class Property {
  public final PropertyKey key;
  
  public final java.util.Optional<hydra.ext.json.model.Value> value;
  
  public Property (PropertyKey key, java.util.Optional<hydra.ext.json.model.Value> value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Property)) {
      return false;
    }
    Property o = (Property) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public Property withKey(PropertyKey key) {
    return new Property(key, value);
  }
  
  public Property withValue(java.util.Optional<hydra.ext.json.model.Value> value) {
    return new Property(key, value);
  }
}