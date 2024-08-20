// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.queries;

import java.io.Serializable;

public class Projection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/queries.Projection");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.ext.tinkerpop.queries.Expression value;
  
  public final hydra.util.Opt<hydra.ext.tinkerpop.queries.Variable> as;
  
  public Projection (hydra.ext.tinkerpop.queries.Expression value, hydra.util.Opt<hydra.ext.tinkerpop.queries.Variable> as) {
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((as));
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
  
  public Projection withValue(hydra.ext.tinkerpop.queries.Expression value) {
    java.util.Objects.requireNonNull((value));
    return new Projection(value, as);
  }
  
  public Projection withAs(hydra.util.Opt<hydra.ext.tinkerpop.queries.Variable> as) {
    java.util.Objects.requireNonNull((as));
    return new Projection(value, as);
  }
}
