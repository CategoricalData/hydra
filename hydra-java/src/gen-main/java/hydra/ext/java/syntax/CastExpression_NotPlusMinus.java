package hydra.ext.java.syntax;

public class CastExpression_NotPlusMinus {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.CastExpression.NotPlusMinus");
  
  public final hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds;
  
  public final hydra.ext.java.syntax.UnaryExpression expression;
  
  public CastExpression_NotPlusMinus (hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds, hydra.ext.java.syntax.UnaryExpression expression) {
    this.refAndBounds = refAndBounds;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastExpression_NotPlusMinus)) {
      return false;
    }
    CastExpression_NotPlusMinus o = (CastExpression_NotPlusMinus) (other);
    return refAndBounds.equals(o.refAndBounds) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * refAndBounds.hashCode() + 3 * expression.hashCode();
  }
  
  public CastExpression_NotPlusMinus withRefAndBounds(hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds) {
    return new CastExpression_NotPlusMinus(refAndBounds, expression);
  }
  
  public CastExpression_NotPlusMinus withExpression(hydra.ext.java.syntax.UnaryExpression expression) {
    return new CastExpression_NotPlusMinus(refAndBounds, expression);
  }
}