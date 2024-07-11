// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class ScopeStringArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.ScopeStringArgument");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalScopeArgument scope;
  
  public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> strings;
  
  public ScopeStringArgument (hydra.langs.tinkerpop.gremlin.TraversalScopeArgument scope, java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> strings) {
    java.util.Objects.requireNonNull((scope));
    java.util.Objects.requireNonNull((strings));
    this.scope = scope;
    this.strings = strings;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScopeStringArgument)) {
      return false;
    }
    ScopeStringArgument o = (ScopeStringArgument) (other);
    return scope.equals(o.scope) && strings.equals(o.strings);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * strings.hashCode();
  }
  
  public ScopeStringArgument withScope(hydra.langs.tinkerpop.gremlin.TraversalScopeArgument scope) {
    java.util.Objects.requireNonNull((scope));
    return new ScopeStringArgument(scope, strings);
  }
  
  public ScopeStringArgument withStrings(java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> strings) {
    java.util.Objects.requireNonNull((strings));
    return new ScopeStringArgument(scope, strings);
  }
}