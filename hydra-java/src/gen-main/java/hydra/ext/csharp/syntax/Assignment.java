// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class Assignment implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.Assignment");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.UnaryExpression left;
  
  public final hydra.ext.csharp.syntax.AssignmentOperator operator;
  
  public final hydra.ext.csharp.syntax.Expression right;
  
  public Assignment (hydra.ext.csharp.syntax.UnaryExpression left, hydra.ext.csharp.syntax.AssignmentOperator operator, hydra.ext.csharp.syntax.Expression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.operator = operator;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Assignment)) {
      return false;
    }
    Assignment o = (Assignment) (other);
    return left.equals(o.left) && operator.equals(o.operator) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * operator.hashCode() + 5 * right.hashCode();
  }
  
  public Assignment withLeft(hydra.ext.csharp.syntax.UnaryExpression left) {
    java.util.Objects.requireNonNull((left));
    return new Assignment(left, operator, right);
  }
  
  public Assignment withOperator(hydra.ext.csharp.syntax.AssignmentOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new Assignment(left, operator, right);
  }
  
  public Assignment withRight(hydra.ext.csharp.syntax.Expression right) {
    java.util.Objects.requireNonNull((right));
    return new Assignment(left, operator, right);
  }
}