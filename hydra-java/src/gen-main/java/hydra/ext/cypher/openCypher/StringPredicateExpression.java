// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class StringPredicateExpression implements Serializable, Comparable<StringPredicateExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.StringPredicateExpression");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final hydra.ext.cypher.openCypher.StringPredicateOperator operator;

  public final hydra.ext.cypher.openCypher.AddOrSubtractExpression expression;

  public StringPredicateExpression (hydra.ext.cypher.openCypher.StringPredicateOperator operator, hydra.ext.cypher.openCypher.AddOrSubtractExpression expression) {
    this.operator = operator;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringPredicateExpression)) {
      return false;
    }
    StringPredicateExpression o = (StringPredicateExpression) other;
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
  public int compareTo(StringPredicateExpression other) {
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

  public StringPredicateExpression withOperator(hydra.ext.cypher.openCypher.StringPredicateOperator operator) {
    return new StringPredicateExpression(operator, expression);
  }

  public StringPredicateExpression withExpression(hydra.ext.cypher.openCypher.AddOrSubtractExpression expression) {
    return new StringPredicateExpression(operator, expression);
  }
}
