// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class RangeArgument implements Serializable, Comparable<RangeArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.RangeArgument");

  public static final hydra.core.Name MIN = new hydra.core.Name("min");

  public static final hydra.core.Name MAX = new hydra.core.Name("max");

  public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument min;

  public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument max;

  public RangeArgument (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument min, hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument max) {
    this.min = min;
    this.max = max;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RangeArgument)) {
      return false;
    }
    RangeArgument o = (RangeArgument) other;
    return java.util.Objects.equals(
      this.min,
      o.min) && java.util.Objects.equals(
      this.max,
      o.max);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(min) + 3 * java.util.Objects.hashCode(max);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RangeArgument other) {
    int cmp = 0;
    cmp = ((Comparable) min).compareTo(other.min);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) max).compareTo(other.max);
  }

  public RangeArgument withMin(hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument min) {
    return new RangeArgument(min, max);
  }

  public RangeArgument withMax(hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument max) {
    return new RangeArgument(min, max);
  }
}
