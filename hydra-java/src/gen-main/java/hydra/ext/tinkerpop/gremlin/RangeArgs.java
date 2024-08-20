// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public class RangeArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.RangeArgs");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FIELD_NAME_MIN = new hydra.core.Name("min");
  
  public static final hydra.core.Name FIELD_NAME_MAX = new hydra.core.Name("max");
  
  public final hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.ext.tinkerpop.gremlin.IntegerArgument min;
  
  public final hydra.ext.tinkerpop.gremlin.IntegerArgument max;
  
  public RangeArgs (hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.ext.tinkerpop.gremlin.IntegerArgument min, hydra.ext.tinkerpop.gremlin.IntegerArgument max) {
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
  
  public RangeArgs withScope(hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new RangeArgs(scope, min, max);
  }
  
  public RangeArgs withMin(hydra.ext.tinkerpop.gremlin.IntegerArgument min) {
    java.util.Objects.requireNonNull((min));
    return new RangeArgs(scope, min, max);
  }
  
  public RangeArgs withMax(hydra.ext.tinkerpop.gremlin.IntegerArgument max) {
    java.util.Objects.requireNonNull((max));
    return new RangeArgs(scope, min, max);
  }
}
