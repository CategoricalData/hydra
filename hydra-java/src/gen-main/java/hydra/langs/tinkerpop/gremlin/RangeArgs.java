// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class RangeArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.RangeArgs");
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.langs.tinkerpop.gremlin.IntegerArgument min;
  
  public final hydra.langs.tinkerpop.gremlin.IntegerArgument max;
  
  public RangeArgs (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.langs.tinkerpop.gremlin.IntegerArgument min, hydra.langs.tinkerpop.gremlin.IntegerArgument max) {
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    if (min == null) {
      throw new IllegalArgumentException("null value for 'min' argument");
    }
    if (max == null) {
      throw new IllegalArgumentException("null value for 'max' argument");
    }
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
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    return new RangeArgs(scope, min, max);
  }
  
  public RangeArgs withMin(hydra.langs.tinkerpop.gremlin.IntegerArgument min) {
    if (min == null) {
      throw new IllegalArgumentException("null value for 'min' argument");
    }
    return new RangeArgs(scope, min, max);
  }
  
  public RangeArgs withMax(hydra.langs.tinkerpop.gremlin.IntegerArgument max) {
    if (max == null) {
      throw new IllegalArgumentException("null value for 'max' argument");
    }
    return new RangeArgs(scope, min, max);
  }
}