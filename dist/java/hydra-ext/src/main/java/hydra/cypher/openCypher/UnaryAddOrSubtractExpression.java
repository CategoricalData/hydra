// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class UnaryAddOrSubtractExpression implements Serializable, Comparable<UnaryAddOrSubtractExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.UnaryAddOrSubtractExpression");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final hydra.util.Maybe<hydra.cypher.openCypher.AddOrSubtractOperator> operator;

  public final hydra.cypher.openCypher.NonArithmeticOperatorExpression expression;

  public UnaryAddOrSubtractExpression (hydra.util.Maybe<hydra.cypher.openCypher.AddOrSubtractOperator> operator, hydra.cypher.openCypher.NonArithmeticOperatorExpression expression) {
    this.operator = operator;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnaryAddOrSubtractExpression)) {
      return false;
    }
    UnaryAddOrSubtractExpression o = (UnaryAddOrSubtractExpression) other;
    return java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operator) + 3 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnaryAddOrSubtractExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      operator,
      other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public UnaryAddOrSubtractExpression withOperator(hydra.util.Maybe<hydra.cypher.openCypher.AddOrSubtractOperator> operator) {
    return new UnaryAddOrSubtractExpression(operator, expression);
  }

  public UnaryAddOrSubtractExpression withExpression(hydra.cypher.openCypher.NonArithmeticOperatorExpression expression) {
    return new UnaryAddOrSubtractExpression(operator, expression);
  }
}
