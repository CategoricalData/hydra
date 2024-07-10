// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class OptionalTraversalScopeArgumentAndIntegerArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.OptionalTraversalScopeArgumentAndIntegerArgument");
  
  public final java.util.Optional<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.langs.tinkerpop.gremlin.IntegerArgument long_;
  
  public OptionalTraversalScopeArgumentAndIntegerArgument (java.util.Optional<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.langs.tinkerpop.gremlin.IntegerArgument long_) {
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    if (long_ == null) {
      throw new IllegalArgumentException("null value for 'long' argument");
    }
    this.scope = scope;
    this.long_ = long_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OptionalTraversalScopeArgumentAndIntegerArgument)) {
      return false;
    }
    OptionalTraversalScopeArgumentAndIntegerArgument o = (OptionalTraversalScopeArgumentAndIntegerArgument) (other);
    return scope.equals(o.scope) && long_.equals(o.long_);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * long_.hashCode();
  }
  
  public OptionalTraversalScopeArgumentAndIntegerArgument withScope(java.util.Optional<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    return new OptionalTraversalScopeArgumentAndIntegerArgument(scope, long_);
  }
  
  public OptionalTraversalScopeArgumentAndIntegerArgument withLong(hydra.langs.tinkerpop.gremlin.IntegerArgument long_) {
    if (long_ == null) {
      throw new IllegalArgumentException("null value for 'long' argument");
    }
    return new OptionalTraversalScopeArgumentAndIntegerArgument(scope, long_);
  }
}