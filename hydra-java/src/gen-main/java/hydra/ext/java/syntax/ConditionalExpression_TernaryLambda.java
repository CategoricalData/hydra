package hydra.ext.java.syntax;

public class ConditionalExpression_TernaryLambda {
  public final hydra.ext.java.syntax.ConditionalOrExpression cond;
  
  public final hydra.ext.java.syntax.Expression ifTrue;
  
  public final hydra.ext.java.syntax.LambdaExpression ifFalse;
  
  public ConditionalExpression_TernaryLambda (hydra.ext.java.syntax.ConditionalOrExpression cond, hydra.ext.java.syntax.Expression ifTrue, hydra.ext.java.syntax.LambdaExpression ifFalse) {
    this.cond = cond;
    this.ifTrue = ifTrue;
    this.ifFalse = ifFalse;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConditionalExpression_TernaryLambda)) {
      return false;
    }
    ConditionalExpression_TernaryLambda o = (ConditionalExpression_TernaryLambda) (other);
    return cond.equals(o.cond) && ifTrue.equals(o.ifTrue) && ifFalse.equals(o.ifFalse);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * ifTrue.hashCode() + 5 * ifFalse.hashCode();
  }
  
  public ConditionalExpression_TernaryLambda withCond(hydra.ext.java.syntax.ConditionalOrExpression cond) {
    return new ConditionalExpression_TernaryLambda(cond, ifTrue, ifFalse);
  }
  
  public ConditionalExpression_TernaryLambda withIfTrue(hydra.ext.java.syntax.Expression ifTrue) {
    return new ConditionalExpression_TernaryLambda(cond, ifTrue, ifFalse);
  }
  
  public ConditionalExpression_TernaryLambda withIfFalse(hydra.ext.java.syntax.LambdaExpression ifFalse) {
    return new ConditionalExpression_TernaryLambda(cond, ifTrue, ifFalse);
  }
}