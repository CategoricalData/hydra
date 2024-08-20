// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public class ReplaceArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.ReplaceArgs");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FIELD_NAME_FROM = new hydra.core.Name("from");
  
  public static final hydra.core.Name FIELD_NAME_TO = new hydra.core.Name("to");
  
  public final hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.ext.tinkerpop.gremlin.StringNullableArgument from;
  
  public final hydra.ext.tinkerpop.gremlin.StringNullableArgument to;
  
  public ReplaceArgs (hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.ext.tinkerpop.gremlin.StringNullableArgument from, hydra.ext.tinkerpop.gremlin.StringNullableArgument to) {
    java.util.Objects.requireNonNull((scope));
    java.util.Objects.requireNonNull((from));
    java.util.Objects.requireNonNull((to));
    this.scope = scope;
    this.from = from;
    this.to = to;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReplaceArgs)) {
      return false;
    }
    ReplaceArgs o = (ReplaceArgs) (other);
    return scope.equals(o.scope) && from.equals(o.from) && to.equals(o.to);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * from.hashCode() + 5 * to.hashCode();
  }
  
  public ReplaceArgs withScope(hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new ReplaceArgs(scope, from, to);
  }
  
  public ReplaceArgs withFrom(hydra.ext.tinkerpop.gremlin.StringNullableArgument from) {
    java.util.Objects.requireNonNull((from));
    return new ReplaceArgs(scope, from, to);
  }
  
  public ReplaceArgs withTo(hydra.ext.tinkerpop.gremlin.StringNullableArgument to) {
    java.util.Objects.requireNonNull((to));
    return new ReplaceArgs(scope, from, to);
  }
}
