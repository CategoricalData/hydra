// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class Projection implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.Projection");
  
  public final hydra.langs.tinkerpop.queries.Expression value;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.queries.Variable> as;
  
  public Projection (hydra.langs.tinkerpop.queries.Expression value, hydra.util.Opt<hydra.langs.tinkerpop.queries.Variable> as) {
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
  
  public Projection withValue(hydra.langs.tinkerpop.queries.Expression value) {
    java.util.Objects.requireNonNull((value));
    return new Projection(value, as);
  }
  
  public Projection withAs(hydra.util.Opt<hydra.langs.tinkerpop.queries.Variable> as) {
    java.util.Objects.requireNonNull((as));
    return new Projection(value, as);
  }
}