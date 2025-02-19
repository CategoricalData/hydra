// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class RefConditionalExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RefConditionalExpression");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_TRUE = new hydra.core.Name("true");
  
  public static final hydra.core.Name FIELD_NAME_FALSE = new hydra.core.Name("false");
  
  public final hydra.ext.csharp.syntax.NullCoalescingExpression condition;
  
  public final hydra.ext.csharp.syntax.VariableReference true_;
  
  public final hydra.ext.csharp.syntax.VariableReference false_;
  
  public RefConditionalExpression (hydra.ext.csharp.syntax.NullCoalescingExpression condition, hydra.ext.csharp.syntax.VariableReference true_, hydra.ext.csharp.syntax.VariableReference false_) {
    java.util.Objects.requireNonNull((condition));
    java.util.Objects.requireNonNull((true_));
    java.util.Objects.requireNonNull((false_));
    this.condition = condition;
    this.true_ = true_;
    this.false_ = false_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RefConditionalExpression)) {
      return false;
    }
    RefConditionalExpression o = (RefConditionalExpression) (other);
    return condition.equals(o.condition) && true_.equals(o.true_) && false_.equals(o.false_);
  }
  
  @Override
  public int hashCode() {
    return 2 * condition.hashCode() + 3 * true_.hashCode() + 5 * false_.hashCode();
  }
  
  public RefConditionalExpression withCondition(hydra.ext.csharp.syntax.NullCoalescingExpression condition) {
    java.util.Objects.requireNonNull((condition));
    return new RefConditionalExpression(condition, true_, false_);
  }
  
  public RefConditionalExpression withTrue(hydra.ext.csharp.syntax.VariableReference true_) {
    java.util.Objects.requireNonNull((true_));
    return new RefConditionalExpression(condition, true_, false_);
  }
  
  public RefConditionalExpression withFalse(hydra.ext.csharp.syntax.VariableReference false_) {
    java.util.Objects.requireNonNull((false_));
    return new RefConditionalExpression(condition, true_, false_);
  }
}