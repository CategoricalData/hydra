// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ConditionalExpression_TernaryCond implements Serializable, Comparable<ConditionalExpression_TernaryCond> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ConditionalExpression_TernaryCond");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_IF_TRUE = new hydra.core.Name("ifTrue");
  
  public static final hydra.core.Name FIELD_NAME_IF_FALSE = new hydra.core.Name("ifFalse");
  
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
    ConditionalExpression_TernaryCond o = (ConditionalExpression_TernaryCond) other;
    return java.util.Objects.equals(
      this.cond,
      o.cond) && java.util.Objects.equals(
      this.ifTrue,
      o.ifTrue) && java.util.Objects.equals(
      this.ifFalse,
      o.ifFalse);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(cond) + 3 * java.util.Objects.hashCode(ifTrue) + 5 * java.util.Objects.hashCode(ifFalse);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConditionalExpression_TernaryCond other) {
    int cmp = 0;
    cmp = ((Comparable) cond).compareTo(other.cond);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) ifTrue).compareTo(other.ifTrue);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) ifFalse).compareTo(other.ifFalse);
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
