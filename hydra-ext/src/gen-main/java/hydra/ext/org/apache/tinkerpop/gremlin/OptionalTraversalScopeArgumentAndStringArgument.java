// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class OptionalTraversalScopeArgumentAndStringArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public final hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument string;
  
  public OptionalTraversalScopeArgumentAndStringArgument (hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.ext.org.apache.tinkerpop.gremlin.StringArgument string) {
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
  
  public OptionalTraversalScopeArgumentAndStringArgument withScope(hydra.util.Opt<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new OptionalTraversalScopeArgumentAndStringArgument(scope, string);
  }
  
  public OptionalTraversalScopeArgumentAndStringArgument withString(hydra.ext.org.apache.tinkerpop.gremlin.StringArgument string) {
    java.util.Objects.requireNonNull((string));
    return new OptionalTraversalScopeArgumentAndStringArgument(scope, string);
  }
}