package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RangeLiteral implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RangeLiteral");
  
  public final java.util.Optional<java.math.BigInteger> start;
  
  public final java.util.Optional<java.math.BigInteger> end;
  
  public RangeLiteral (java.util.Optional<java.math.BigInteger> start, java.util.Optional<java.math.BigInteger> end) {
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
  
  public RangeLiteral withStart(java.util.Optional<java.math.BigInteger> start) {
    return new RangeLiteral(start, end);
  }
  
  public RangeLiteral withEnd(java.util.Optional<java.math.BigInteger> end) {
    return new RangeLiteral(start, end);
  }
}