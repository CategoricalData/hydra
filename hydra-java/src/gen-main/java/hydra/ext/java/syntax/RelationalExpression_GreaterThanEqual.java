package hydra.ext.java.syntax;

public class RelationalExpression_GreaterThanEqual {
  public final RelationalExpression lhs;
  
  public final ShiftExpression rhs;
  
  public RelationalExpression_GreaterThanEqual (RelationalExpression lhs, ShiftExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationalExpression_GreaterThanEqual)) {
      return false;
    }
    RelationalExpression_GreaterThanEqual o = (RelationalExpression_GreaterThanEqual) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public RelationalExpression_GreaterThanEqual withLhs(RelationalExpression lhs) {
    return new RelationalExpression_GreaterThanEqual(lhs, rhs);
  }
  
  public RelationalExpression_GreaterThanEqual withRhs(ShiftExpression rhs) {
    return new RelationalExpression_GreaterThanEqual(lhs, rhs);
  }
}