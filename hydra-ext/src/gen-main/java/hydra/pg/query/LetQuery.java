// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class LetQuery implements Serializable, Comparable<LetQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.LetQuery");
  
  public static final hydra.core.Name BINDINGS = new hydra.core.Name("bindings");
  
  public static final hydra.core.Name ENVIRONMENT = new hydra.core.Name("environment");
  
  public final java.util.List<hydra.pg.query.Binding> bindings;
  
  public final hydra.pg.query.Query environment;
  
  public LetQuery (java.util.List<hydra.pg.query.Binding> bindings, hydra.pg.query.Query environment) {
    this.bindings = bindings;
    this.environment = environment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetQuery)) {
      return false;
    }
    LetQuery o = (LetQuery) other;
    return java.util.Objects.equals(
      this.bindings,
      o.bindings) && java.util.Objects.equals(
      this.environment,
      o.environment);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(bindings) + 3 * java.util.Objects.hashCode(environment);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LetQuery other) {
    int cmp = 0;
    cmp = Integer.compare(
      bindings.hashCode(),
      other.bindings.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) environment).compareTo(other.environment);
  }
  
  public LetQuery withBindings(java.util.List<hydra.pg.query.Binding> bindings) {
    return new LetQuery(bindings, environment);
  }
  
  public LetQuery withEnvironment(hydra.pg.query.Query environment) {
    return new LetQuery(bindings, environment);
  }
}
