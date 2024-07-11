// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RangeLiteral implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RangeLiteral");
  
  public final hydra.util.Opt<java.math.BigInteger> start;
  
  public final hydra.util.Opt<java.math.BigInteger> end;
  
  public RangeLiteral (hydra.util.Opt<java.math.BigInteger> start, hydra.util.Opt<java.math.BigInteger> end) {
    java.util.Objects.requireNonNull((start));
    java.util.Objects.requireNonNull((end));
    this.start = start;
    this.end = end;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RangeLiteral)) {
      return false;
    }
    RangeLiteral o = (RangeLiteral) (other);
    return start.equals(o.start) && end.equals(o.end);
  }
  
  @Override
  public int hashCode() {
    return 2 * start.hashCode() + 3 * end.hashCode();
  }
  
  public RangeLiteral withStart(hydra.util.Opt<java.math.BigInteger> start) {
    java.util.Objects.requireNonNull((start));
    return new RangeLiteral(start, end);
  }
  
  public RangeLiteral withEnd(hydra.util.Opt<java.math.BigInteger> end) {
    java.util.Objects.requireNonNull((end));
    return new RangeLiteral(start, end);
  }
}