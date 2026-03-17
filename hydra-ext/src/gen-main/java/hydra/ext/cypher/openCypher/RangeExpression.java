// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class RangeExpression implements Serializable, Comparable<RangeExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.RangeExpression");

  public static final hydra.core.Name START = new hydra.core.Name("start");

  public static final hydra.core.Name END = new hydra.core.Name("end");

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> start;

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> end;

  public RangeExpression (hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> start, hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> end) {
    this.start = start;
    this.end = end;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RangeExpression)) {
      return false;
    }
    RangeExpression o = (RangeExpression) other;
    return java.util.Objects.equals(
      this.start,
      o.start) && java.util.Objects.equals(
      this.end,
      o.end);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(start) + 3 * java.util.Objects.hashCode(end);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RangeExpression other) {
    int cmp = 0;
    cmp = ((Comparable) start).compareTo(other.start);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) end).compareTo(other.end);
  }

  public RangeExpression withStart(hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> start) {
    return new RangeExpression(start, end);
  }

  public RangeExpression withEnd(hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> end) {
    return new RangeExpression(start, end);
  }
}
