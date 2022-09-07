package hydra.ext.java.syntax;

public class ShiftExpression_Binary {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ShiftExpression.Binary");
  
  public final hydra.ext.java.syntax.ShiftExpression lhs;
  
  public final hydra.ext.java.syntax.AdditiveExpression rhs;
  
  public ShiftExpression_Binary (hydra.ext.java.syntax.ShiftExpression lhs, hydra.ext.java.syntax.AdditiveExpression rhs) {
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
  
  public ShiftExpression_Binary withLhs(hydra.ext.java.syntax.ShiftExpression lhs) {
    return new ShiftExpression_Binary(lhs, rhs);
  }
  
  public ShiftExpression_Binary withRhs(hydra.ext.java.syntax.AdditiveExpression rhs) {
    return new ShiftExpression_Binary(lhs, rhs);
  }
}