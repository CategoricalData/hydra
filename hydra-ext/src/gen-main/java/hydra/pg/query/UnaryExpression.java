// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class UnaryExpression implements Serializable, Comparable<UnaryExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.UnaryExpression");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name OPERAND = new hydra.core.Name("operand");

  public final hydra.pg.query.UnaryOperator operator;

  public final hydra.pg.query.Expression operand;

  public UnaryExpression (hydra.pg.query.UnaryOperator operator, hydra.pg.query.Expression operand) {
    this.operator = operator;
    this.operand = operand;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnaryExpression)) {
      return false;
    }
    UnaryExpression o = (UnaryExpression) other;
    return java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.operand,
      o.operand);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operator) + 3 * java.util.Objects.hashCode(operand);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnaryExpression other) {
    int cmp = 0;
    cmp = ((Comparable) operator).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) operand).compareTo(other.operand);
  }

  public UnaryExpression withOperator(hydra.pg.query.UnaryOperator operator) {
    return new UnaryExpression(operator, operand);
  }

  public UnaryExpression withOperand(hydra.pg.query.Expression operand) {
    return new UnaryExpression(operator, operand);
  }
}
