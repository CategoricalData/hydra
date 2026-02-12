// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class SumLhs implements Serializable, Comparable<SumLhs> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SumLhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERAND = new hydra.core.Name("operand");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public final hydra.ext.python.syntax.Sum operand;
  
  public final hydra.ext.python.syntax.SumOp operator;
  
  public SumLhs (hydra.ext.python.syntax.Sum operand, hydra.ext.python.syntax.SumOp operator) {
    this.operand = operand;
    this.operator = operator;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SumLhs)) {
      return false;
    }
    SumLhs o = (SumLhs) other;
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
  public int compareTo(SumLhs other) {
    int cmp = 0;
    cmp = ((Comparable) operand).compareTo(other.operand);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) operator).compareTo(other.operator);
  }
  
  public SumLhs withOperand(hydra.ext.python.syntax.Sum operand) {
    return new SumLhs(operand, operator);
  }
  
  public SumLhs withOperator(hydra.ext.python.syntax.SumOp operator) {
    return new SumLhs(operand, operator);
  }
}
