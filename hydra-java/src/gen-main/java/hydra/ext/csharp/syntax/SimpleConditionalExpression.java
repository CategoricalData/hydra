// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class SimpleConditionalExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SimpleConditionalExpression");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_TRUE = new hydra.core.Name("true");
  
  public static final hydra.core.Name FIELD_NAME_FALSE = new hydra.core.Name("false");
  
  public final hydra.ext.csharp.syntax.NullCoalescingExpression condition;
  
  public final hydra.ext.csharp.syntax.Expression true_;
  
  public final hydra.ext.csharp.syntax.Expression false_;
  
  public SimpleConditionalExpression (hydra.ext.csharp.syntax.NullCoalescingExpression condition, hydra.ext.csharp.syntax.Expression true_, hydra.ext.csharp.syntax.Expression false_) {
    java.util.Objects.requireNonNull((condition));
    java.util.Objects.requireNonNull((true_));
    java.util.Objects.requireNonNull((false_));
    this.condition = condition;
    this.true_ = true_;
    this.false_ = false_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleConditionalExpression)) {
      return false;
    }
    SimpleConditionalExpression o = (SimpleConditionalExpression) (other);
    return condition.equals(o.condition) && true_.equals(o.true_) && false_.equals(o.false_);
  }
  
  @Override
  public int hashCode() {
    return 2 * condition.hashCode() + 3 * true_.hashCode() + 5 * false_.hashCode();
  }
  
  public SimpleConditionalExpression withCondition(hydra.ext.csharp.syntax.NullCoalescingExpression condition) {
    java.util.Objects.requireNonNull((condition));
    return new SimpleConditionalExpression(condition, true_, false_);
  }
  
  public SimpleConditionalExpression withTrue(hydra.ext.csharp.syntax.Expression true_) {
    java.util.Objects.requireNonNull((true_));
    return new SimpleConditionalExpression(condition, true_, false_);
  }
  
  public SimpleConditionalExpression withFalse(hydra.ext.csharp.syntax.Expression false_) {
    java.util.Objects.requireNonNull((false_));
    return new SimpleConditionalExpression(condition, true_, false_);
  }
}