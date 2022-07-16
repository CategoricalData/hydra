package hydra.ext.java.syntax;

public class ConditionalExpression_TernaryLambda {
  public final ConditionalOrExpression cond;
  
  public final Expression ifTrue;
  
  public final LambdaExpression ifFalse;
  
  public ConditionalExpression_TernaryLambda (ConditionalOrExpression cond, Expression ifTrue, LambdaExpression ifFalse) {
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
  
  public ConditionalExpression_TernaryLambda withCond(ConditionalOrExpression cond) {
    return new ConditionalExpression_TernaryLambda(cond, ifTrue, ifFalse);
  }
  
  public ConditionalExpression_TernaryLambda withIfTrue(Expression ifTrue) {
    return new ConditionalExpression_TernaryLambda(cond, ifTrue, ifFalse);
  }
  
  public ConditionalExpression_TernaryLambda withIfFalse(LambdaExpression ifFalse) {
    return new ConditionalExpression_TernaryLambda(cond, ifTrue, ifFalse);
  }
}