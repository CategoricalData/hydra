// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class ReplaceArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.ReplaceArgs");
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.langs.tinkerpop.gremlin.StringNullableArgument from;
  
  public final hydra.langs.tinkerpop.gremlin.StringNullableArgument to;
  
  public ReplaceArgs (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.langs.tinkerpop.gremlin.StringNullableArgument from, hydra.langs.tinkerpop.gremlin.StringNullableArgument to) {
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    if (from == null) {
      throw new IllegalArgumentException("null value for 'from' argument");
    }
    if (to == null) {
      throw new IllegalArgumentException("null value for 'to' argument");
    }
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
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    return new ReplaceArgs(scope, from, to);
  }
  
  public ReplaceArgs withFrom(hydra.langs.tinkerpop.gremlin.StringNullableArgument from) {
    if (from == null) {
      throw new IllegalArgumentException("null value for 'from' argument");
    }
    return new ReplaceArgs(scope, from, to);
  }
  
  public ReplaceArgs withTo(hydra.langs.tinkerpop.gremlin.StringNullableArgument to) {
    if (to == null) {
      throw new IllegalArgumentException("null value for 'to' argument");
    }
    return new ReplaceArgs(scope, from, to);
  }
}