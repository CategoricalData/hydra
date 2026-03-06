// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class Projection implements Serializable, Comparable<Projection> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.Projection");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name AS = new hydra.core.Name("as");
  
  public final hydra.pg.query.Expression value;
  
  public final hydra.util.Maybe<hydra.pg.query.Variable> as;
  
  public Projection (hydra.pg.query.Expression value, hydra.util.Maybe<hydra.pg.query.Variable> as) {
    this.value = value;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Projection)) {
      return false;
    }
    Projection o = (Projection) other;
    return java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.as,
      o.as);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(as);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Projection other) {
    int cmp = 0;
    cmp = ((Comparable) value).compareTo(other.value);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      as.hashCode(),
      other.as.hashCode());
  }
  
  public Projection withValue(hydra.pg.query.Expression value) {
    return new Projection(value, as);
  }
  
  public Projection withAs(hydra.util.Maybe<hydra.pg.query.Variable> as) {
    return new Projection(value, as);
  }
}
