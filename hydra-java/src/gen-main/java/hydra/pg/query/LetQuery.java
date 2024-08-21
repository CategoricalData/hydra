// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class LetQuery implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/pg/query.LetQuery");
  
  public static final hydra.core.Name FIELD_NAME_BINDINGS = new hydra.core.Name("bindings");
  
  public static final hydra.core.Name FIELD_NAME_ENVIRONMENT = new hydra.core.Name("environment");
  
  public final java.util.List<hydra.pg.query.Binding> bindings;
  
  public final hydra.pg.query.Query environment;
  
  public LetQuery (java.util.List<hydra.pg.query.Binding> bindings, hydra.pg.query.Query environment) {
    java.util.Objects.requireNonNull((bindings));
    java.util.Objects.requireNonNull((environment));
    this.bindings = bindings;
    this.environment = environment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetQuery)) {
      return false;
    }
    LetQuery o = (LetQuery) (other);
    return bindings.equals(o.bindings) && environment.equals(o.environment);
  }
  
  @Override
  public int hashCode() {
    return 2 * bindings.hashCode() + 3 * environment.hashCode();
  }
  
  public LetQuery withBindings(java.util.List<hydra.pg.query.Binding> bindings) {
    java.util.Objects.requireNonNull((bindings));
    return new LetQuery(bindings, environment);
  }
  
  public LetQuery withEnvironment(hydra.pg.query.Query environment) {
    java.util.Objects.requireNonNull((environment));
    return new LetQuery(bindings, environment);
  }
}