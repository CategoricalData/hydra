package hydra.ext.java.syntax;

public class ShiftExpression_Binary {
  public final ShiftExpression lhs;
  
  public final AdditiveExpression rhs;
  
  public ShiftExpression_Binary (ShiftExpression lhs, AdditiveExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShiftExpression_Binary)) {
      return false;
    }
    ShiftExpression_Binary o = (ShiftExpression_Binary) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public ShiftExpression_Binary withLhs(ShiftExpression lhs) {
    return new ShiftExpression_Binary(lhs, rhs);
  }
  
  public ShiftExpression_Binary withRhs(AdditiveExpression rhs) {
    return new ShiftExpression_Binary(lhs, rhs);
  }
}