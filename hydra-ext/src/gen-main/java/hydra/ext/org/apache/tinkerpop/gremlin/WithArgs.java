// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class WithArgs implements Serializable, Comparable<WithArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WithArgs");
  
  public static final hydra.core.Name KEYS = new hydra.core.Name("keys");
  
  public static final hydra.core.Name VALUES = new hydra.core.Name("values");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.WithArgsKeys keys;
  
  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.WithArgsValues> values;
  
  public WithArgs (hydra.ext.org.apache.tinkerpop.gremlin.WithArgsKeys keys, hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.WithArgsValues> values) {
    this.keys = keys;
    this.values = values;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WithArgs)) {
      return false;
    }
    WithArgs o = (WithArgs) other;
    return java.util.Objects.equals(
      this.keys,
      o.keys) && java.util.Objects.equals(
      this.values,
      o.values);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(keys) + 3 * java.util.Objects.hashCode(values);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WithArgs other) {
    int cmp = 0;
    cmp = ((Comparable) keys).compareTo(other.keys);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      values.hashCode(),
      other.values.hashCode());
  }
  
  public WithArgs withKeys(hydra.ext.org.apache.tinkerpop.gremlin.WithArgsKeys keys) {
    return new WithArgs(keys, values);
  }
  
  public WithArgs withValues(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.WithArgsValues> values) {
    return new WithArgs(keys, values);
  }
}
