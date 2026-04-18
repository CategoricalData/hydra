// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ConditionalExpression_TernaryCond implements Serializable, Comparable<ConditionalExpression_TernaryCond> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ConditionalExpression_TernaryCond");

  public static final hydra.core.Name COND = new hydra.core.Name("cond");

  public static final hydra.core.Name IF_TRUE = new hydra.core.Name("ifTrue");

  public static final hydra.core.Name IF_FALSE = new hydra.core.Name("ifFalse");

  public final hydra.java.syntax.ConditionalOrExpression cond;

  public final hydra.java.syntax.Expression ifTrue;

  public final hydra.java.syntax.ConditionalExpression ifFalse;

  public ConditionalExpression_TernaryCond (hydra.java.syntax.ConditionalOrExpression cond, hydra.java.syntax.Expression ifTrue, hydra.java.syntax.ConditionalExpression ifFalse) {
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
    cmp = hydra.util.Comparing.compare(
      cond,
      other.cond);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      ifTrue,
      other.ifTrue);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      ifFalse,
      other.ifFalse);
  }

  public ConditionalExpression_TernaryCond withCond(hydra.java.syntax.ConditionalOrExpression cond) {
    return new ConditionalExpression_TernaryCond(cond, ifTrue, ifFalse);
  }

  public ConditionalExpression_TernaryCond withIfTrue(hydra.java.syntax.Expression ifTrue) {
    return new ConditionalExpression_TernaryCond(cond, ifTrue, ifFalse);
  }

  public ConditionalExpression_TernaryCond withIfFalse(hydra.java.syntax.ConditionalExpression ifFalse) {
    return new ConditionalExpression_TernaryCond(cond, ifTrue, ifFalse);
  }
}
