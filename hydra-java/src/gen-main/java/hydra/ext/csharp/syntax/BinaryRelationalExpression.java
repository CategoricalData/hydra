// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryRelationalExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryRelationalExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.RelationalExpression left;
  
  public final hydra.ext.csharp.syntax.RelationalOperator operator;
  
  public final hydra.ext.csharp.syntax.ShiftExpression right;
  
  public BinaryRelationalExpression (hydra.ext.csharp.syntax.RelationalExpression left, hydra.ext.csharp.syntax.RelationalOperator operator, hydra.ext.csharp.syntax.ShiftExpression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.operator = operator;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryRelationalExpression)) {
      return false;
    }
    BinaryRelationalExpression o = (BinaryRelationalExpression) (other);
    return left.equals(o.left) && operator.equals(o.operator) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * operator.hashCode() + 5 * right.hashCode();
  }
  
  public BinaryRelationalExpression withLeft(hydra.ext.csharp.syntax.RelationalExpression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryRelationalExpression(left, operator, right);
  }
  
  public BinaryRelationalExpression withOperator(hydra.ext.csharp.syntax.RelationalOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new BinaryRelationalExpression(left, operator, right);
  }
  
  public BinaryRelationalExpression withRight(hydra.ext.csharp.syntax.ShiftExpression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryRelationalExpression(left, operator, right);
  }
}