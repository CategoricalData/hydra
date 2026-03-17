// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class Quantifier implements Serializable, Comparable<Quantifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.Quantifier");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final hydra.ext.cypher.openCypher.QuantifierOperator operator;

  public final hydra.ext.cypher.openCypher.FilterExpression expression;

  public Quantifier (hydra.ext.cypher.openCypher.QuantifierOperator operator, hydra.ext.cypher.openCypher.FilterExpression expression) {
    this.operator = operator;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Quantifier)) {
      return false;
    }
    Quantifier o = (Quantifier) other;
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
  public int compareTo(Quantifier other) {
    int cmp = 0;
    cmp = ((Comparable) operator).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expression).compareTo(other.expression);
  }

  public Quantifier withOperator(hydra.ext.cypher.openCypher.QuantifierOperator operator) {
    return new Quantifier(operator, expression);
  }

  public Quantifier withExpression(hydra.ext.cypher.openCypher.FilterExpression expression) {
    return new Quantifier(operator, expression);
  }
}
