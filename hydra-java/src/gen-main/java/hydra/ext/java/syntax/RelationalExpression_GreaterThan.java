package hydra.ext.java.syntax;

public class RelationalExpression_GreaterThan {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.RelationalExpression.GreaterThan");
  
  public final hydra.ext.java.syntax.RelationalExpression lhs;
  
  public final hydra.ext.java.syntax.ShiftExpression rhs;
  
  public RelationalExpression_GreaterThan (hydra.ext.java.syntax.RelationalExpression lhs, hydra.ext.java.syntax.ShiftExpression rhs) {
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
  
  public RelationalExpression_GreaterThan withLhs(hydra.ext.java.syntax.RelationalExpression lhs) {
    return new RelationalExpression_GreaterThan(lhs, rhs);
  }
  
  public RelationalExpression_GreaterThan withRhs(hydra.ext.java.syntax.ShiftExpression rhs) {
    return new RelationalExpression_GreaterThan(lhs, rhs);
  }
}