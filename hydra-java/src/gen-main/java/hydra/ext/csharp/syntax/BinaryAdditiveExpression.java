// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryAdditiveExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryAdditiveExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.AdditiveExpression left;
  
  public final hydra.ext.csharp.syntax.AdditiveOperator operator;
  
  public final hydra.ext.csharp.syntax.MultiplicativeExpression right;
  
  public BinaryAdditiveExpression (hydra.ext.csharp.syntax.AdditiveExpression left, hydra.ext.csharp.syntax.AdditiveOperator operator, hydra.ext.csharp.syntax.MultiplicativeExpression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.operator = operator;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryAdditiveExpression)) {
      return false;
    }
    BinaryAdditiveExpression o = (BinaryAdditiveExpression) (other);
    return left.equals(o.left) && operator.equals(o.operator) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * operator.hashCode() + 5 * right.hashCode();
  }
  
  public BinaryAdditiveExpression withLeft(hydra.ext.csharp.syntax.AdditiveExpression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryAdditiveExpression(left, operator, right);
  }
  
  public BinaryAdditiveExpression withOperator(hydra.ext.csharp.syntax.AdditiveOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new BinaryAdditiveExpression(left, operator, right);
  }
  
  public BinaryAdditiveExpression withRight(hydra.ext.csharp.syntax.MultiplicativeExpression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryAdditiveExpression(left, operator, right);
  }
}