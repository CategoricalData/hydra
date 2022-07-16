package hydra.ext.java.syntax;

public class CastExpression_NotPlusMinus {
  public final CastExpression_RefAndBounds refAndBounds;
  
  public final UnaryExpression expression;
  
  public CastExpression_NotPlusMinus (CastExpression_RefAndBounds refAndBounds, UnaryExpression expression) {
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
  
  public CastExpression_NotPlusMinus withRefAndBounds(CastExpression_RefAndBounds refAndBounds) {
    return new CastExpression_NotPlusMinus(refAndBounds, expression);
  }
  
  public CastExpression_NotPlusMinus withExpression(UnaryExpression expression) {
    return new CastExpression_NotPlusMinus(refAndBounds, expression);
  }
}