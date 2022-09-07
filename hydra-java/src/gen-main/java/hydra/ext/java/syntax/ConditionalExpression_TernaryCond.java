package hydra.ext.java.syntax;

public class ConditionalExpression_TernaryCond {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ConditionalExpression.TernaryCond");
  
  public final hydra.ext.java.syntax.ConditionalOrExpression cond;
  
  public final hydra.ext.java.syntax.Expression ifTrue;
  
  public final hydra.ext.java.syntax.ConditionalExpression ifFalse;
  
  public ConditionalExpression_TernaryCond (hydra.ext.java.syntax.ConditionalOrExpression cond, hydra.ext.java.syntax.Expression ifTrue, hydra.ext.java.syntax.ConditionalExpression ifFalse) {
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
  
  public ConditionalExpression_TernaryCond withCond(hydra.ext.java.syntax.ConditionalOrExpression cond) {
    return new ConditionalExpression_TernaryCond(cond, ifTrue, ifFalse);
  }
  
  public ConditionalExpression_TernaryCond withIfTrue(hydra.ext.java.syntax.Expression ifTrue) {
    return new ConditionalExpression_TernaryCond(cond, ifTrue, ifFalse);
  }
  
  public ConditionalExpression_TernaryCond withIfFalse(hydra.ext.java.syntax.ConditionalExpression ifFalse) {
    return new ConditionalExpression_TernaryCond(cond, ifTrue, ifFalse);
  }
}