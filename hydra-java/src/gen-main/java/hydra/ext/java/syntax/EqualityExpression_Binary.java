package hydra.ext.java.syntax;

public class EqualityExpression_Binary {
  public final hydra.ext.java.syntax.EqualityExpression lhs;
  
  public final hydra.ext.java.syntax.RelationalExpression rhs;
  
  public EqualityExpression_Binary (hydra.ext.java.syntax.EqualityExpression lhs, hydra.ext.java.syntax.RelationalExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EqualityExpression_Binary)) {
      return false;
    }
    EqualityExpression_Binary o = (EqualityExpression_Binary) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public EqualityExpression_Binary withLhs(hydra.ext.java.syntax.EqualityExpression lhs) {
    return new EqualityExpression_Binary(lhs, rhs);
  }
  
  public EqualityExpression_Binary withRhs(hydra.ext.java.syntax.RelationalExpression rhs) {
    return new EqualityExpression_Binary(lhs, rhs);
  }
}