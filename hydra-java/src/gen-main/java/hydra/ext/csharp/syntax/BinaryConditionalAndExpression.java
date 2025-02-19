// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryConditionalAndExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryConditionalAndExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.ConditionalAndExpression left;
  
  public final hydra.ext.csharp.syntax.InclusiveOrExpression right;
  
  public BinaryConditionalAndExpression (hydra.ext.csharp.syntax.ConditionalAndExpression left, hydra.ext.csharp.syntax.InclusiveOrExpression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryConditionalAndExpression)) {
      return false;
    }
    BinaryConditionalAndExpression o = (BinaryConditionalAndExpression) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public BinaryConditionalAndExpression withLeft(hydra.ext.csharp.syntax.ConditionalAndExpression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryConditionalAndExpression(left, right);
  }
  
  public BinaryConditionalAndExpression withRight(hydra.ext.csharp.syntax.InclusiveOrExpression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryConditionalAndExpression(left, right);
  }
}