package hydra.ext.java.syntax;

public class RelationalExpression_GreaterThan {
  public final RelationalExpression lhs;
  
  public final ShiftExpression rhs;
  
  public RelationalExpression_GreaterThan (RelationalExpression lhs, ShiftExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationalExpression_GreaterThan)) {
      return false;
    }
    RelationalExpression_GreaterThan o = (RelationalExpression_GreaterThan) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public RelationalExpression_GreaterThan withLhs(RelationalExpression lhs) {
    return new RelationalExpression_GreaterThan(lhs, rhs);
  }
  
  public RelationalExpression_GreaterThan withRhs(ShiftExpression rhs) {
    return new RelationalExpression_GreaterThan(lhs, rhs);
  }
}