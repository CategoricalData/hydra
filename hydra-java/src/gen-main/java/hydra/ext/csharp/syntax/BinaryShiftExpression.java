// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryShiftExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryShiftExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.ShiftExpression left;
  
  public final hydra.ext.csharp.syntax.ShiftOperator operator;
  
  public final hydra.ext.csharp.syntax.AdditiveExpression right;
  
  public BinaryShiftExpression (hydra.ext.csharp.syntax.ShiftExpression left, hydra.ext.csharp.syntax.ShiftOperator operator, hydra.ext.csharp.syntax.AdditiveExpression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.operator = operator;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryShiftExpression)) {
      return false;
    }
    BinaryShiftExpression o = (BinaryShiftExpression) (other);
    return left.equals(o.left) && operator.equals(o.operator) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * operator.hashCode() + 5 * right.hashCode();
  }
  
  public BinaryShiftExpression withLeft(hydra.ext.csharp.syntax.ShiftExpression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryShiftExpression(left, operator, right);
  }
  
  public BinaryShiftExpression withOperator(hydra.ext.csharp.syntax.ShiftOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new BinaryShiftExpression(left, operator, right);
  }
  
  public BinaryShiftExpression withRight(hydra.ext.csharp.syntax.AdditiveExpression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryShiftExpression(left, operator, right);
  }
}