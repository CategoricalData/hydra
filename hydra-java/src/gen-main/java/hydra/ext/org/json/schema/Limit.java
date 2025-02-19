// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public class Limit implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.Limit");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_EXCLUSIVE = new hydra.core.Name("exclusive");
  
  public final Integer value;
  
  public final Boolean exclusive;
  
  public Limit (Integer value, Boolean exclusive) {
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((exclusive));
    this.value = value;
    this.exclusive = exclusive;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Limit)) {
      return false;
    }
    Limit o = (Limit) (other);
    return value.equals(o.value) && exclusive.equals(o.exclusive);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode() + 3 * exclusive.hashCode();
  }
  
  public Limit withValue(Integer value) {
    java.util.Objects.requireNonNull((value));
    return new Limit(value, exclusive);
  }
  
  public Limit withExclusive(Boolean exclusive) {
    java.util.Objects.requireNonNull((exclusive));
    return new Limit(value, exclusive);
  }
}