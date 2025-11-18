// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import hydra.util.Maybe;

import java.io.Serializable;
import java.math.BigInteger;

public class RangeLiteral implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.RangeLiteral");
  
  public static final hydra.core.Name FIELD_NAME_START = new hydra.core.Name("start");
  
  public static final hydra.core.Name FIELD_NAME_END = new hydra.core.Name("end");
  
  public final Maybe<BigInteger> start;
  
  public final Maybe<BigInteger> end;
  
  public RangeLiteral (Maybe<BigInteger> start, Maybe<BigInteger> end) {
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
  
  public RangeLiteral withStart(Maybe<BigInteger> start) {
    java.util.Objects.requireNonNull((start));
    return new RangeLiteral(start, end);
  }
  
  public RangeLiteral withEnd(Maybe<BigInteger> end) {
    java.util.Objects.requireNonNull((end));
    return new RangeLiteral(start, end);
  }
}
