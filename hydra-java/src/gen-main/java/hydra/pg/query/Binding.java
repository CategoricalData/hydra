// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class Binding implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.query.Binding");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.pg.query.Variable key;
  
  public final hydra.pg.query.Query value;
  
  public Binding (hydra.pg.query.Variable key, hydra.pg.query.Query value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Binding)) {
      return false;
    }
    Binding o = (Binding) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public Binding withKey(hydra.pg.query.Variable key) {
    java.util.Objects.requireNonNull((key));
    return new Binding(key, value);
  }
  
  public Binding withValue(hydra.pg.query.Query value) {
    java.util.Objects.requireNonNull((value));
    return new Binding(key, value);
  }
}