// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class RangeArgs implements Serializable, Comparable<RangeArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.RangeArgs");

  public static final hydra.core.Name SCOPE = new hydra.core.Name("scope");

  public static final hydra.core.Name MIN = new hydra.core.Name("min");

  public static final hydra.core.Name MAX = new hydra.core.Name("max");

  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope;

  public final hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument min;

  public final hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument max;

  public RangeArgs (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument min, hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument max) {
    this.scope = scope;
    this.min = min;
    this.max = max;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RangeArgs)) {
      return false;
    }
    RangeArgs o = (RangeArgs) other;
    return java.util.Objects.equals(
      this.scope,
      o.scope) && java.util.Objects.equals(
      this.min,
      o.min) && java.util.Objects.equals(
      this.max,
      o.max);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(scope) + 3 * java.util.Objects.hashCode(min) + 5 * java.util.Objects.hashCode(max);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RangeArgs other) {
    int cmp = 0;
    cmp = ((Comparable) scope).compareTo(other.scope);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) min).compareTo(other.min);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) max).compareTo(other.max);
  }

  public RangeArgs withScope(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    return new RangeArgs(scope, min, max);
  }

  public RangeArgs withMin(hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument min) {
    return new RangeArgs(scope, min, max);
  }

  public RangeArgs withMax(hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument max) {
    return new RangeArgs(scope, min, max);
  }
}
