// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class RangeLiteral implements Serializable, Comparable<RangeLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.RangeLiteral");
  
  public static final hydra.core.Name START = new hydra.core.Name("start");
  
  public static final hydra.core.Name END = new hydra.core.Name("end");
  
  public final hydra.util.Maybe<java.math.BigInteger> start;
  
  public final hydra.util.Maybe<java.math.BigInteger> end;
  
  public RangeLiteral (hydra.util.Maybe<java.math.BigInteger> start, hydra.util.Maybe<java.math.BigInteger> end) {
    this.start = start;
    this.end = end;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RangeLiteral)) {
      return false;
    }
    RangeLiteral o = (RangeLiteral) other;
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
  public int compareTo(RangeLiteral other) {
    int cmp = 0;
    cmp = Integer.compare(
      start.hashCode(),
      other.start.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      end.hashCode(),
      other.end.hashCode());
  }
  
  public RangeLiteral withStart(hydra.util.Maybe<java.math.BigInteger> start) {
    return new RangeLiteral(start, end);
  }
  
  public RangeLiteral withEnd(hydra.util.Maybe<java.math.BigInteger> end) {
    return new RangeLiteral(start, end);
  }
}
