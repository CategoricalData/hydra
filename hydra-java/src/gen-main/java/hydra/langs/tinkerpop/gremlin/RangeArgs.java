// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class RangeArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.RangeArgs");
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.langs.tinkerpop.gremlin.IntegerArgument min;
  
  public final hydra.langs.tinkerpop.gremlin.IntegerArgument max;
  
  public RangeArgs (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.langs.tinkerpop.gremlin.IntegerArgument min, hydra.langs.tinkerpop.gremlin.IntegerArgument max) {
    java.util.Objects.requireNonNull((scope));
    java.util.Objects.requireNonNull((min));
    java.util.Objects.requireNonNull((max));
    this.scope = scope;
    this.min = min;
    this.max = max;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RangeArgs)) {
      return false;
    }
    RangeArgs o = (RangeArgs) (other);
    return scope.equals(o.scope) && min.equals(o.min) && max.equals(o.max);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * min.hashCode() + 5 * max.hashCode();
  }
  
  public RangeArgs withScope(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new RangeArgs(scope, min, max);
  }
  
  public RangeArgs withMin(hydra.langs.tinkerpop.gremlin.IntegerArgument min) {
    java.util.Objects.requireNonNull((min));
    return new RangeArgs(scope, min, max);
  }
  
  public RangeArgs withMax(hydra.langs.tinkerpop.gremlin.IntegerArgument max) {
    java.util.Objects.requireNonNull((max));
    return new RangeArgs(scope, min, max);
  }
}