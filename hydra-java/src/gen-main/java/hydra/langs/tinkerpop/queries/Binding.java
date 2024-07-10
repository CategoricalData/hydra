// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class Binding implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.Binding");
  
  public final hydra.langs.tinkerpop.queries.Variable key;
  
  public final hydra.langs.tinkerpop.queries.Query value;
  
  public Binding (hydra.langs.tinkerpop.queries.Variable key, hydra.langs.tinkerpop.queries.Query value) {
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
  
  public Binding withKey(hydra.langs.tinkerpop.queries.Variable key) {
    if (key == null) {
      throw new IllegalArgumentException("null value for 'key' argument");
    }
    return new Binding(key, value);
  }
  
  public Binding withValue(hydra.langs.tinkerpop.queries.Query value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    return new Binding(key, value);
  }
}