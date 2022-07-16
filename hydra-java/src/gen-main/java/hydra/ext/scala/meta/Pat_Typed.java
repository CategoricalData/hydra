package hydra.ext.scala.meta;

public class Pat_Typed {
  public final Pat lhs;
  
  public final Type rhs;
  
  public Pat_Typed (Pat lhs, Type rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Typed)) {
      return false;
    }
    Pat_Typed o = (Pat_Typed) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Pat_Typed withLhs(Pat lhs) {
    return new Pat_Typed(lhs, rhs);
  }
  
  public Pat_Typed withRhs(Type rhs) {
    return new Pat_Typed(lhs, rhs);
  }
}