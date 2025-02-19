// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class OptionalTraversalScopeArgumentAndIntegerArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FIELD_NAME_LONG = new hydra.core.Name("long");
  
  public final hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument long_;
  
  public OptionalTraversalScopeArgumentAndIntegerArgument (hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument long_) {
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
  
  public OptionalTraversalScopeArgumentAndIntegerArgument withScope(hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new OptionalTraversalScopeArgumentAndIntegerArgument(scope, long_);
  }
  
  public OptionalTraversalScopeArgumentAndIntegerArgument withLong(hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument long_) {
    java.util.Objects.requireNonNull((long_));
    return new OptionalTraversalScopeArgumentAndIntegerArgument(scope, long_);
  }
}