package hydra.ext.java.syntax;

public class RelationalExpression_GreaterThanEqual {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.RelationalExpression.GreaterThanEqual");
  
  public final hydra.ext.java.syntax.RelationalExpression lhs;
  
  public final hydra.ext.java.syntax.ShiftExpression rhs;
  
  public RelationalExpression_GreaterThanEqual (hydra.ext.java.syntax.RelationalExpression lhs, hydra.ext.java.syntax.ShiftExpression rhs) {
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
  
  public RelationalExpression_GreaterThanEqual withLhs(hydra.ext.java.syntax.RelationalExpression lhs) {
    return new RelationalExpression_GreaterThanEqual(lhs, rhs);
  }
  
  public RelationalExpression_GreaterThanEqual withRhs(hydra.ext.java.syntax.ShiftExpression rhs) {
    return new RelationalExpression_GreaterThanEqual(lhs, rhs);
  }
}