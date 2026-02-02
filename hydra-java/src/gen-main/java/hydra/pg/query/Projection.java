// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class Projection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.query.Projection");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.pg.query.Expression value;
  
  public final hydra.util.Maybe<hydra.pg.query.Variable> as;
  
  public Projection (hydra.pg.query.Expression value, hydra.util.Maybe<hydra.pg.query.Variable> as) {
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
  
  public Projection withValue(hydra.pg.query.Expression value) {
    java.util.Objects.requireNonNull((value));
    return new Projection(value, as);
  }
  
  public Projection withAs(hydra.util.Maybe<hydra.pg.query.Variable> as) {
    java.util.Objects.requireNonNull((as));
    return new Projection(value, as);
  }
}
