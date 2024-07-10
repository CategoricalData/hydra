// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class LetQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.LetQuery");
  
  public final java.util.List<hydra.langs.tinkerpop.queries.Binding> bindings;
  
  public final hydra.langs.tinkerpop.queries.Query environment;
  
  public LetQuery (java.util.List<hydra.langs.tinkerpop.queries.Binding> bindings, hydra.langs.tinkerpop.queries.Query environment) {
    if (bindings == null) {
      throw new IllegalArgumentException("null value for 'bindings' argument");
    }
    if (environment == null) {
      throw new IllegalArgumentException("null value for 'environment' argument");
    }
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
  
  public LetQuery withBindings(java.util.List<hydra.langs.tinkerpop.queries.Binding> bindings) {
    if (bindings == null) {
      throw new IllegalArgumentException("null value for 'bindings' argument");
    }
    return new LetQuery(bindings, environment);
  }
  
  public LetQuery withEnvironment(hydra.langs.tinkerpop.queries.Query environment) {
    if (environment == null) {
      throw new IllegalArgumentException("null value for 'environment' argument");
    }
    return new LetQuery(bindings, environment);
  }
}