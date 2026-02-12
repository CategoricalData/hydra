// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TermLhs implements Serializable, Comparable<TermLhs> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TermLhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERAND = new hydra.core.Name("operand");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public final hydra.ext.python.syntax.Term operand;
  
  public final hydra.ext.python.syntax.TermOp operator;
  
  public TermLhs (hydra.ext.python.syntax.Term operand, hydra.ext.python.syntax.TermOp operator) {
    this.operand = operand;
    this.operator = operator;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermLhs)) {
      return false;
    }
    TermLhs o = (TermLhs) other;
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
  public int compareTo(TermLhs other) {
    int cmp = 0;
    cmp = ((Comparable) operand).compareTo(other.operand);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) operator).compareTo(other.operator);
  }
  
  public TermLhs withOperand(hydra.ext.python.syntax.Term operand) {
    return new TermLhs(operand, operator);
  }
  
  public TermLhs withOperator(hydra.ext.python.syntax.TermOp operator) {
    return new TermLhs(operand, operator);
  }
}
