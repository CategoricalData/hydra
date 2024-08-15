// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TailArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TailArgs");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER = new hydra.core.Name("integer");
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.IntegerArgument> integer;
  
  public TailArgs (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.util.Opt<hydra.langs.tinkerpop.gremlin.IntegerArgument> integer) {
    java.util.Objects.requireNonNull((scope));
    java.util.Objects.requireNonNull((integer));
    this.scope = scope;
    this.integer = integer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TailArgs)) {
      return false;
    }
    TailArgs o = (TailArgs) (other);
    return scope.equals(o.scope) && integer.equals(o.integer);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * integer.hashCode();
  }
  
  public TailArgs withScope(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new TailArgs(scope, integer);
  }
  
  public TailArgs withInteger(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.IntegerArgument> integer) {
    java.util.Objects.requireNonNull((integer));
    return new TailArgs(scope, integer);
  }
}