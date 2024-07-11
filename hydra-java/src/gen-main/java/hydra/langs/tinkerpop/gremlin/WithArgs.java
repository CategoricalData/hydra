// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class WithArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.WithArgs");
  
  public final hydra.langs.tinkerpop.gremlin.WithArgsKeys keys;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.WithArgsValues> values;
  
  public WithArgs (hydra.langs.tinkerpop.gremlin.WithArgsKeys keys, hydra.util.Opt<hydra.langs.tinkerpop.gremlin.WithArgsValues> values) {
    java.util.Objects.requireNonNull((keys));
    java.util.Objects.requireNonNull((values));
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
    java.util.Objects.requireNonNull((keys));
    return new WithArgs(keys, values);
  }
  
  public WithArgs withValues(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.WithArgsValues> values) {
    java.util.Objects.requireNonNull((values));
    return new WithArgs(keys, values);
  }
}