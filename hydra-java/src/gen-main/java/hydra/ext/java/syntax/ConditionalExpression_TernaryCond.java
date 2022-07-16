package hydra.ext.java.syntax;

public class ConditionalExpression_TernaryCond {
  public final ConditionalOrExpression cond;
  
  public final Expression ifTrue;
  
  public final ConditionalExpression ifFalse;
  
  public ConditionalExpression_TernaryCond (ConditionalOrExpression cond, Expression ifTrue, ConditionalExpression ifFalse) {
    this.cond = cond;
    this.ifTrue = ifTrue;
    this.ifFalse = ifFalse;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConditionalExpression_TernaryCond)) {
      return false;
    }
    ConditionalExpression_TernaryCond o = (ConditionalExpression_TernaryCond) (other);
    return cond.equals(o.cond) && ifTrue.equals(o.ifTrue) && ifFalse.equals(o.ifFalse);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * ifTrue.hashCode() + 5 * ifFalse.hashCode();
  }
  
  public ConditionalExpression_TernaryCond withCond(ConditionalOrExpression cond) {
    return new ConditionalExpression_TernaryCond(cond, ifTrue, ifFalse);
  }
  
  public ConditionalExpression_TernaryCond withIfTrue(Expression ifTrue) {
    return new ConditionalExpression_TernaryCond(cond, ifTrue, ifFalse);
  }
  
  public ConditionalExpression_TernaryCond withIfFalse(ConditionalExpression ifFalse) {
    return new ConditionalExpression_TernaryCond(cond, ifTrue, ifFalse);
  }
}