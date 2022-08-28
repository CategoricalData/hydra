package hydra.ext.java.syntax;

public class CastExpression_Lambda {
  public final hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds;
  
  public final hydra.ext.java.syntax.LambdaExpression expression;
  
  public CastExpression_Lambda (hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds, hydra.ext.java.syntax.LambdaExpression expression) {
    this.refAndBounds = refAndBounds;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastExpression_Lambda)) {
      return false;
    }
    CastExpression_Lambda o = (CastExpression_Lambda) (other);
    return refAndBounds.equals(o.refAndBounds) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * refAndBounds.hashCode() + 3 * expression.hashCode();
  }
  
  public CastExpression_Lambda withRefAndBounds(hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds) {
    return new CastExpression_Lambda(refAndBounds, expression);
  }
  
  public CastExpression_Lambda withExpression(hydra.ext.java.syntax.LambdaExpression expression) {
    return new CastExpression_Lambda(refAndBounds, expression);
  }
}