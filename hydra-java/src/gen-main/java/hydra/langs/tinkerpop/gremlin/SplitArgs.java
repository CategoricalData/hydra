// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class SplitArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.SplitArgs");
  
  public final java.util.Optional<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.langs.tinkerpop.gremlin.StringNullableArgument delimiter;
  
  public SplitArgs (java.util.Optional<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.langs.tinkerpop.gremlin.StringNullableArgument delimiter) {
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    if (delimiter == null) {
      throw new IllegalArgumentException("null value for 'delimiter' argument");
    }
    this.scope = scope;
    this.delimiter = delimiter;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SplitArgs)) {
      return false;
    }
    SplitArgs o = (SplitArgs) (other);
    return scope.equals(o.scope) && delimiter.equals(o.delimiter);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * delimiter.hashCode();
  }
  
  public SplitArgs withScope(java.util.Optional<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    return new SplitArgs(scope, delimiter);
  }
  
  public SplitArgs withDelimiter(hydra.langs.tinkerpop.gremlin.StringNullableArgument delimiter) {
    if (delimiter == null) {
      throw new IllegalArgumentException("null value for 'delimiter' argument");
    }
    return new SplitArgs(scope, delimiter);
  }
}