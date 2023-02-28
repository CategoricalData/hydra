package hydra.langs.pegasus.pdl;

public class Property {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/pegasus/pdl.Property");
  
  public final hydra.langs.pegasus.pdl.PropertyKey key;
  
  public final java.util.Optional<hydra.langs.json.model.Value> value;
  
  public Property (hydra.langs.pegasus.pdl.PropertyKey key, java.util.Optional<hydra.langs.json.model.Value> value) {
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
  
  public Property withKey(hydra.langs.pegasus.pdl.PropertyKey key) {
    return new Property(key, value);
  }
  
  public Property withValue(java.util.Optional<hydra.langs.json.model.Value> value) {
    return new Property(key, value);
  }
}