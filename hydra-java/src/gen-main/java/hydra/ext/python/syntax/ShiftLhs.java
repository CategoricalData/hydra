// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ShiftLhs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ShiftLhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERAND = new hydra.core.Name("operand");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public final hydra.ext.python.syntax.ShiftExpression operand;
  
  public final hydra.ext.python.syntax.ShiftOp operator;
  
  public ShiftLhs (hydra.ext.python.syntax.ShiftExpression operand, hydra.ext.python.syntax.ShiftOp operator) {
    java.util.Objects.requireNonNull((operand));
    java.util.Objects.requireNonNull((operator));
    this.operand = operand;
    this.operator = operator;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShiftLhs)) {
      return false;
    }
    ShiftLhs o = (ShiftLhs) (other);
    return operand.equals(o.operand) && operator.equals(o.operator);
  }
  
  @Override
  public int hashCode() {
    return 2 * operand.hashCode() + 3 * operator.hashCode();
  }
  
  public ShiftLhs withOperand(hydra.ext.python.syntax.ShiftExpression operand) {
    java.util.Objects.requireNonNull((operand));
    return new ShiftLhs(operand, operator);
  }
  
  public ShiftLhs withOperator(hydra.ext.python.syntax.ShiftOp operator) {
    java.util.Objects.requireNonNull((operator));
    return new ShiftLhs(operand, operator);
  }
}