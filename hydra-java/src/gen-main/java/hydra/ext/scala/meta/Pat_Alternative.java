package hydra.ext.scala.meta;

public class Pat_Alternative {
  public final Pat lhs;
  
  public final Pat rhs;
  
  public Pat_Alternative (Pat lhs, Pat rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Alternative)) {
      return false;
    }
    Pat_Alternative o = (Pat_Alternative) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Pat_Alternative withLhs(Pat lhs) {
    return new Pat_Alternative(lhs, rhs);
  }
  
  public Pat_Alternative withRhs(Pat rhs) {
    return new Pat_Alternative(lhs, rhs);
  }
}