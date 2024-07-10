// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class WithArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.WithArgs");
  
  public final hydra.langs.tinkerpop.gremlin.WithArgsKeys keys;
  
  public final java.util.Optional<hydra.langs.tinkerpop.gremlin.WithArgsValues> values;
  
  public WithArgs (hydra.langs.tinkerpop.gremlin.WithArgsKeys keys, java.util.Optional<hydra.langs.tinkerpop.gremlin.WithArgsValues> values) {
    if (keys == null) {
      throw new IllegalArgumentException("null value for 'keys' argument");
    }
    if (values == null) {
      throw new IllegalArgumentException("null value for 'values' argument");
    }
    this.keys = keys;
    this.values = values;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WithArgs)) {
      return false;
    }
    WithArgs o = (WithArgs) (other);
    return keys.equals(o.keys) && values.equals(o.values);
  }
  
  @Override
  public int hashCode() {
    return 2 * keys.hashCode() + 3 * values.hashCode();
  }
  
  public WithArgs withKeys(hydra.langs.tinkerpop.gremlin.WithArgsKeys keys) {
    if (keys == null) {
      throw new IllegalArgumentException("null value for 'keys' argument");
    }
    return new WithArgs(keys, values);
  }
  
  public WithArgs withValues(java.util.Optional<hydra.langs.tinkerpop.gremlin.WithArgsValues> values) {
    if (values == null) {
      throw new IllegalArgumentException("null value for 'values' argument");
    }
    return new WithArgs(keys, values);
  }
}