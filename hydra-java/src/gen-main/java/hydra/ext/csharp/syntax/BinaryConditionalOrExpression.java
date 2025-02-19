// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryConditionalOrExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryConditionalOrExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.ConditionalOrExpression left;
  
  public final hydra.ext.csharp.syntax.ConditionalAndExpression right;
  
  public BinaryConditionalOrExpression (hydra.ext.csharp.syntax.ConditionalOrExpression left, hydra.ext.csharp.syntax.ConditionalAndExpression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryConditionalOrExpression)) {
      return false;
    }
    BinaryConditionalOrExpression o = (BinaryConditionalOrExpression) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public BinaryConditionalOrExpression withLeft(hydra.ext.csharp.syntax.ConditionalOrExpression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryConditionalOrExpression(left, right);
  }
  
  public BinaryConditionalOrExpression withRight(hydra.ext.csharp.syntax.ConditionalAndExpression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryConditionalOrExpression(left, right);
  }
}