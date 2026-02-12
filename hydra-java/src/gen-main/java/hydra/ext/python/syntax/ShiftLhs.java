// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ShiftLhs implements Serializable, Comparable<ShiftLhs> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ShiftLhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERAND = new hydra.core.Name("operand");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public final hydra.ext.python.syntax.ShiftExpression operand;
  
  public final hydra.ext.python.syntax.ShiftOp operator;
  
  public ShiftLhs (hydra.ext.python.syntax.ShiftExpression operand, hydra.ext.python.syntax.ShiftOp operator) {
    this.operand = operand;
    this.operator = operator;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShiftLhs)) {
      return false;
    }
    ShiftLhs o = (ShiftLhs) other;
    return java.util.Objects.equals(
      this.operand,
      o.operand) && java.util.Objects.equals(
      this.operator,
      o.operator);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operand) + 3 * java.util.Objects.hashCode(operator);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ShiftLhs other) {
    int cmp = 0;
    cmp = ((Comparable) operand).compareTo(other.operand);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) operator).compareTo(other.operator);
  }
  
  public ShiftLhs withOperand(hydra.ext.python.syntax.ShiftExpression operand) {
    return new ShiftLhs(operand, operator);
  }
  
  public ShiftLhs withOperator(hydra.ext.python.syntax.ShiftOp operator) {
    return new ShiftLhs(operand, operator);
  }
}
