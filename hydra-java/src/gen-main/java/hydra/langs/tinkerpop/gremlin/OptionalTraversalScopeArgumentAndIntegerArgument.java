// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class OptionalTraversalScopeArgumentAndIntegerArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.OptionalTraversalScopeArgumentAndIntegerArgument");
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.langs.tinkerpop.gremlin.IntegerArgument long_;
  
  public OptionalTraversalScopeArgumentAndIntegerArgument (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.langs.tinkerpop.gremlin.IntegerArgument long_) {
    java.util.Objects.requireNonNull((scope));
    java.util.Objects.requireNonNull((long_));
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
  
  public OptionalTraversalScopeArgumentAndIntegerArgument withScope(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new OptionalTraversalScopeArgumentAndIntegerArgument(scope, long_);
  }
  
  public OptionalTraversalScopeArgumentAndIntegerArgument withLong(hydra.langs.tinkerpop.gremlin.IntegerArgument long_) {
    java.util.Objects.requireNonNull((long_));
    return new OptionalTraversalScopeArgumentAndIntegerArgument(scope, long_);
  }
}