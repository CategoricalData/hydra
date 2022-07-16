package hydra.ext.scala.meta;

public class Pat_Bind {
  public final Pat lhs;
  
  public final Pat rhs;
  
  public Pat_Bind (Pat lhs, Pat rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Bind)) {
      return false;
    }
    Pat_Bind o = (Pat_Bind) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Pat_Bind withLhs(Pat lhs) {
    return new Pat_Bind(lhs, rhs);
  }
  
  public Pat_Bind withRhs(Pat rhs) {
    return new Pat_Bind(lhs, rhs);
  }
}