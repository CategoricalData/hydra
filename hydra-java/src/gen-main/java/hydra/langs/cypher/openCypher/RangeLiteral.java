package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RangeLiteral implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RangeLiteral");
  
  public final java.util.Optional<Integer> from;
  
  public final java.util.Optional<Integer> to;
  
  public RangeLiteral (java.util.Optional<Integer> from, java.util.Optional<Integer> to) {
    this.from = from;
    this.to = to;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RangeLiteral)) {
      return false;
    }
    RangeLiteral o = (RangeLiteral) (other);
    return from.equals(o.from) && to.equals(o.to);
  }
  
  @Override
  public int hashCode() {
    return 2 * from.hashCode() + 3 * to.hashCode();
  }
  
  public RangeLiteral withFrom(java.util.Optional<Integer> from) {
    return new RangeLiteral(from, to);
  }
  
  public RangeLiteral withTo(java.util.Optional<Integer> to) {
    return new RangeLiteral(from, to);
  }
}