// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryMultiplicativeExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryMultiplicativeExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.MultiplicativeExpression left;
  
  public final hydra.ext.csharp.syntax.MultiplicativeOperator operator;
  
  public final hydra.ext.csharp.syntax.UnaryExpression right;
  
  public BinaryMultiplicativeExpression (hydra.ext.csharp.syntax.MultiplicativeExpression left, hydra.ext.csharp.syntax.MultiplicativeOperator operator, hydra.ext.csharp.syntax.UnaryExpression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.operator = operator;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryMultiplicativeExpression)) {
      return false;
    }
    BinaryMultiplicativeExpression o = (BinaryMultiplicativeExpression) (other);
    return left.equals(o.left) && operator.equals(o.operator) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * operator.hashCode() + 5 * right.hashCode();
  }
  
  public BinaryMultiplicativeExpression withLeft(hydra.ext.csharp.syntax.MultiplicativeExpression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryMultiplicativeExpression(left, operator, right);
  }
  
  public BinaryMultiplicativeExpression withOperator(hydra.ext.csharp.syntax.MultiplicativeOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new BinaryMultiplicativeExpression(left, operator, right);
  }
  
  public BinaryMultiplicativeExpression withRight(hydra.ext.csharp.syntax.UnaryExpression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryMultiplicativeExpression(left, operator, right);
  }
}