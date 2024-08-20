// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.queries;

import java.io.Serializable;

public class Binding implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/queries.Binding");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.tinkerpop.queries.Variable key;
  
  public final hydra.ext.tinkerpop.queries.Query value;
  
  public Binding (hydra.ext.tinkerpop.queries.Variable key, hydra.ext.tinkerpop.queries.Query value) {
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
  
  public Binding withKey(hydra.ext.tinkerpop.queries.Variable key) {
    java.util.Objects.requireNonNull((key));
    return new Binding(key, value);
  }
  
  public Binding withValue(hydra.ext.tinkerpop.queries.Query value) {
    java.util.Objects.requireNonNull((value));
    return new Binding(key, value);
  }
}
