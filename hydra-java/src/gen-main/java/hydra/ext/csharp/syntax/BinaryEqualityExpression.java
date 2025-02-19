// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryEqualityExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryEqualityExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.EqualityExpression left;
  
  public final hydra.ext.csharp.syntax.EqualityOperator operator;
  
  public final hydra.ext.csharp.syntax.RelationalExpression right;
  
  public BinaryEqualityExpression (hydra.ext.csharp.syntax.EqualityExpression left, hydra.ext.csharp.syntax.EqualityOperator operator, hydra.ext.csharp.syntax.RelationalExpression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.operator = operator;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryEqualityExpression)) {
      return false;
    }
    BinaryEqualityExpression o = (BinaryEqualityExpression) (other);
    return left.equals(o.left) && operator.equals(o.operator) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * operator.hashCode() + 5 * right.hashCode();
  }
  
  public BinaryEqualityExpression withLeft(hydra.ext.csharp.syntax.EqualityExpression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryEqualityExpression(left, operator, right);
  }
  
  public BinaryEqualityExpression withOperator(hydra.ext.csharp.syntax.EqualityOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new BinaryEqualityExpression(left, operator, right);
  }
  
  public BinaryEqualityExpression withRight(hydra.ext.csharp.syntax.RelationalExpression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryEqualityExpression(left, operator, right);
  }
}