// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class UnaryAddOrSubtractExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.UnaryAddOrSubtractExpression");
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.AddOrSubtractOperator> operator;
  
  public final hydra.langs.cypher.openCypher.NonArithmeticOperatorExpression expression;
  
  public UnaryAddOrSubtractExpression (java.util.Optional<hydra.langs.cypher.openCypher.AddOrSubtractOperator> operator, hydra.langs.cypher.openCypher.NonArithmeticOperatorExpression expression) {
    if (operator == null) {
      throw new IllegalArgumentException("null value for 'operator' argument");
    }
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
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
  
  public UnaryAddOrSubtractExpression withOperator(java.util.Optional<hydra.langs.cypher.openCypher.AddOrSubtractOperator> operator) {
    if (operator == null) {
      throw new IllegalArgumentException("null value for 'operator' argument");
    }
    return new UnaryAddOrSubtractExpression(operator, expression);
  }
  
  public UnaryAddOrSubtractExpression withExpression(hydra.langs.cypher.openCypher.NonArithmeticOperatorExpression expression) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    return new UnaryAddOrSubtractExpression(operator, expression);
  }
}