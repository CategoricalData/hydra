// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class SubstringArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.SubstringArgs");
  
  public final java.util.Optional<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.langs.tinkerpop.gremlin.IntegerArgument start;
  
  public final java.util.Optional<hydra.langs.tinkerpop.gremlin.IntegerArgument> end;
  
  public SubstringArgs (java.util.Optional<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.langs.tinkerpop.gremlin.IntegerArgument start, java.util.Optional<hydra.langs.tinkerpop.gremlin.IntegerArgument> end) {
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    if (start == null) {
      throw new IllegalArgumentException("null value for 'start' argument");
    }
    if (end == null) {
      throw new IllegalArgumentException("null value for 'end' argument");
    }
    this.scope = scope;
    this.start = start;
    this.end = end;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubstringArgs)) {
      return false;
    }
    SubstringArgs o = (SubstringArgs) (other);
    return scope.equals(o.scope) && start.equals(o.start) && end.equals(o.end);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * start.hashCode() + 5 * end.hashCode();
  }
  
  public SubstringArgs withScope(java.util.Optional<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    return new SubstringArgs(scope, start, end);
  }
  
  public SubstringArgs withStart(hydra.langs.tinkerpop.gremlin.IntegerArgument start) {
    if (start == null) {
      throw new IllegalArgumentException("null value for 'start' argument");
    }
    return new SubstringArgs(scope, start, end);
  }
  
  public SubstringArgs withEnd(java.util.Optional<hydra.langs.tinkerpop.gremlin.IntegerArgument> end) {
    if (end == null) {
      throw new IllegalArgumentException("null value for 'end' argument");
    }
    return new SubstringArgs(scope, start, end);
  }
}