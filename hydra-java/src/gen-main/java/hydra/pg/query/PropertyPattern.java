// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class PropertyPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.query.PropertyPattern");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.pg.model.PropertyKey key;
  
  public final hydra.pg.query.PropertyValuePattern value;
  
  public PropertyPattern (hydra.pg.model.PropertyKey key, hydra.pg.query.PropertyValuePattern value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyPattern)) {
      return false;
    }
    PropertyPattern o = (PropertyPattern) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public PropertyPattern withKey(hydra.pg.model.PropertyKey key) {
    java.util.Objects.requireNonNull((key));
    return new PropertyPattern(key, value);
  }
  
  public PropertyPattern withValue(hydra.pg.query.PropertyValuePattern value) {
    java.util.Objects.requireNonNull((value));
    return new PropertyPattern(key, value);
  }
}
