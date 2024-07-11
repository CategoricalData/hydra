// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class ReplaceArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.ReplaceArgs");
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.langs.tinkerpop.gremlin.StringNullableArgument from;
  
  public final hydra.langs.tinkerpop.gremlin.StringNullableArgument to;
  
  public ReplaceArgs (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.langs.tinkerpop.gremlin.StringNullableArgument from, hydra.langs.tinkerpop.gremlin.StringNullableArgument to) {
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
  
  public ReplaceArgs withScope(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new ReplaceArgs(scope, from, to);
  }
  
  public ReplaceArgs withFrom(hydra.langs.tinkerpop.gremlin.StringNullableArgument from) {
    java.util.Objects.requireNonNull((from));
    return new ReplaceArgs(scope, from, to);
  }
  
  public ReplaceArgs withTo(hydra.langs.tinkerpop.gremlin.StringNullableArgument to) {
    java.util.Objects.requireNonNull((to));
    return new ReplaceArgs(scope, from, to);
  }
}