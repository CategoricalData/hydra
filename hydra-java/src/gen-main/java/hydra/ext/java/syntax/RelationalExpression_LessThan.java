package hydra.ext.java.syntax;

public class RelationalExpression_LessThan {
  public final RelationalExpression lhs;
  
  public final ShiftExpression rhs;
  
  public RelationalExpression_LessThan (RelationalExpression lhs, ShiftExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationalExpression_LessThan)) {
      return false;
    }
    RelationalExpression_LessThan o = (RelationalExpression_LessThan) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public RelationalExpression_LessThan withLhs(RelationalExpression lhs) {
    return new RelationalExpression_LessThan(lhs, rhs);
  }
  
  public RelationalExpression_LessThan withRhs(ShiftExpression rhs) {
    return new RelationalExpression_LessThan(lhs, rhs);
  }
}