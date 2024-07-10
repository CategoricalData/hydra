// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class Projection implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.Projection");
  
  public final hydra.langs.tinkerpop.queries.Expression value;
  
  public final java.util.Optional<hydra.langs.tinkerpop.queries.Variable> as;
  
  public Projection (hydra.langs.tinkerpop.queries.Expression value, java.util.Optional<hydra.langs.tinkerpop.queries.Variable> as) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    if (as == null) {
      throw new IllegalArgumentException("null value for 'as' argument");
    }
    this.value = value;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Projection)) {
      return false;
    }
    Projection o = (Projection) (other);
    return value.equals(o.value) && as.equals(o.as);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode() + 3 * as.hashCode();
  }
  
  public Projection withValue(hydra.langs.tinkerpop.queries.Expression value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    return new Projection(value, as);
  }
  
  public Projection withAs(java.util.Optional<hydra.langs.tinkerpop.queries.Variable> as) {
    if (as == null) {
      throw new IllegalArgumentException("null value for 'as' argument");
    }
    return new Projection(value, as);
  }
}