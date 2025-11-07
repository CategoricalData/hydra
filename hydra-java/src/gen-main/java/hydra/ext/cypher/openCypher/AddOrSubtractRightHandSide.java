// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class AddOrSubtractRightHandSide implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.AddOrSubtractRightHandSide");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.cypher.openCypher.AddOrSubtractOperator operator;
  
  public final hydra.ext.cypher.openCypher.MultiplyDivideModuloExpression expression;
  
  public AddOrSubtractRightHandSide (hydra.ext.cypher.openCypher.AddOrSubtractOperator operator, hydra.ext.cypher.openCypher.MultiplyDivideModuloExpression expression) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((expression));
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AddOrSubtractRightHandSide)) {
      return false;
    }
    AddOrSubtractRightHandSide o = (AddOrSubtractRightHandSide) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public AddOrSubtractRightHandSide withOperator(hydra.ext.cypher.openCypher.AddOrSubtractOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new AddOrSubtractRightHandSide(operator, expression);
  }
  
  public AddOrSubtractRightHandSide withExpression(hydra.ext.cypher.openCypher.MultiplyDivideModuloExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new AddOrSubtractRightHandSide(operator, expression);
  }
}
