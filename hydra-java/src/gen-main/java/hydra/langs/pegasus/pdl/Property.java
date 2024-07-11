// Note: this is an automatically generated file. Do not edit.

package hydra.langs.pegasus.pdl;

import java.io.Serializable;

public class Property implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/pegasus/pdl.Property");
  
  public final hydra.langs.pegasus.pdl.PropertyKey key;
  
  public final hydra.util.Opt<hydra.json.Value> value;
  
  public Property (hydra.langs.pegasus.pdl.PropertyKey key, hydra.util.Opt<hydra.json.Value> value) {
    if (key == null) {
      throw new IllegalArgumentException("null value for 'key' argument");
    }
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
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
    if (key == null) {
      throw new IllegalArgumentException("null value for 'key' argument");
    }
    return new Property(key, value);
  }
  
  public Property withValue(hydra.util.Opt<hydra.json.Value> value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    return new Property(key, value);
  }
}