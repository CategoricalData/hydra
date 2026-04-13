// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class TermLhs implements Serializable, Comparable<TermLhs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.TermLhs");

  public static final hydra.core.Name OPERAND = new hydra.core.Name("operand");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public final hydra.python.syntax.Term operand;

  public final hydra.python.syntax.TermOp operator;

  public TermLhs (hydra.python.syntax.Term operand, hydra.python.syntax.TermOp operator) {
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
    cmp = hydra.util.Comparing.compare(
      operand,
      other.operand);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      operator,
      other.operator);
  }

  public TermLhs withOperand(hydra.python.syntax.Term operand) {
    return new TermLhs(operand, operator);
  }

  public TermLhs withOperator(hydra.python.syntax.TermOp operator) {
    return new TermLhs(operand, operator);
  }
}
