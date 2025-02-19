// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class ScopeStringArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ScopeStringArgument");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FIELD_NAME_STRINGS = new hydra.core.Name("strings");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument scope;
  
  public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> strings;
  
  public ScopeStringArgument (hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument scope, java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> strings) {
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
  
  public ScopeStringArgument withScope(hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument scope) {
    java.util.Objects.requireNonNull((scope));
    return new ScopeStringArgument(scope, strings);
  }
  
  public ScopeStringArgument withStrings(java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> strings) {
    java.util.Objects.requireNonNull((strings));
    return new ScopeStringArgument(scope, strings);
  }
}