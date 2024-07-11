// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class OptionalTraversalScopeArgumentAndStringArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.OptionalTraversalScopeArgumentAndStringArgument");
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.langs.tinkerpop.gremlin.StringArgument string;
  
  public OptionalTraversalScopeArgumentAndStringArgument (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.langs.tinkerpop.gremlin.StringArgument string) {
    java.util.Objects.requireNonNull((scope));
    java.util.Objects.requireNonNull((string));
    this.scope = scope;
    this.string = string;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OptionalTraversalScopeArgumentAndStringArgument)) {
      return false;
    }
    OptionalTraversalScopeArgumentAndStringArgument o = (OptionalTraversalScopeArgumentAndStringArgument) (other);
    return scope.equals(o.scope) && string.equals(o.string);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * string.hashCode();
  }
  
  public OptionalTraversalScopeArgumentAndStringArgument withScope(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new OptionalTraversalScopeArgumentAndStringArgument(scope, string);
  }
  
  public OptionalTraversalScopeArgumentAndStringArgument withString(hydra.langs.tinkerpop.gremlin.StringArgument string) {
    java.util.Objects.requireNonNull((string));
    return new OptionalTraversalScopeArgumentAndStringArgument(scope, string);
  }
}