// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import hydra.util.Maybe;

import java.io.Serializable;

public class UnaryAddOrSubtractExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.UnaryAddOrSubtractExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final Maybe<AddOrSubtractOperator> operator;
  
  public final hydra.ext.cypher.openCypher.NonArithmeticOperatorExpression expression;
  
  public UnaryAddOrSubtractExpression (Maybe<AddOrSubtractOperator> operator, hydra.ext.cypher.openCypher.NonArithmeticOperatorExpression expression) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((expression));
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnaryAddOrSubtractExpression)) {
      return false;
    }
    UnaryAddOrSubtractExpression o = (UnaryAddOrSubtractExpression) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public UnaryAddOrSubtractExpression withOperator(Maybe<AddOrSubtractOperator> operator) {
    java.util.Objects.requireNonNull((operator));
    return new UnaryAddOrSubtractExpression(operator, expression);
  }
  
  public UnaryAddOrSubtractExpression withExpression(hydra.ext.cypher.openCypher.NonArithmeticOperatorExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new UnaryAddOrSubtractExpression(operator, expression);
  }
}
