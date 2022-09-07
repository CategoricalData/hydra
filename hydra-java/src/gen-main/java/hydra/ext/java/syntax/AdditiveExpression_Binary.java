package hydra.ext.java.syntax;

public class AdditiveExpression_Binary {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.AdditiveExpression.Binary");
  
  public final hydra.ext.java.syntax.AdditiveExpression lhs;
  
  public final hydra.ext.java.syntax.MultiplicativeExpression rhs;
  
  public AdditiveExpression_Binary (hydra.ext.java.syntax.AdditiveExpression lhs, hydra.ext.java.syntax.MultiplicativeExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdditiveExpression_Binary)) {
      return false;
    }
    AdditiveExpression_Binary o = (AdditiveExpression_Binary) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public AdditiveExpression_Binary withLhs(hydra.ext.java.syntax.AdditiveExpression lhs) {
    return new AdditiveExpression_Binary(lhs, rhs);
  }
  
  public AdditiveExpression_Binary withRhs(hydra.ext.java.syntax.MultiplicativeExpression rhs) {
    return new AdditiveExpression_Binary(lhs, rhs);
  }
}