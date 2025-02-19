// Note: this is an automatically generated file. Do not edit.

package hydra.ext.pegasus.pdl;

import java.io.Serializable;

public class Property implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.pegasus.pdl.Property");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.pegasus.pdl.PropertyKey key;
  
  public final hydra.util.Opt<hydra.json.Value> value;
  
  public Property (hydra.ext.pegasus.pdl.PropertyKey key, hydra.util.Opt<hydra.json.Value> value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
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
  
  public Property withKey(hydra.ext.pegasus.pdl.PropertyKey key) {
    java.util.Objects.requireNonNull((key));
    return new Property(key, value);
  }
  
  public Property withValue(hydra.util.Opt<hydra.json.Value> value) {
    java.util.Objects.requireNonNull((value));
    return new Property(key, value);
  }
}