// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class MultiplyDivideModuloRightHandSide implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.MultiplyDivideModuloRightHandSide");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.cypher.openCypher.MultiplyDivideModuloOperator operator;
  
  public final hydra.ext.cypher.openCypher.PowerOfExpression expression;
  
  public MultiplyDivideModuloRightHandSide (hydra.ext.cypher.openCypher.MultiplyDivideModuloOperator operator, hydra.ext.cypher.openCypher.PowerOfExpression expression) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((expression));
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiplyDivideModuloRightHandSide)) {
      return false;
    }
    MultiplyDivideModuloRightHandSide o = (MultiplyDivideModuloRightHandSide) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public MultiplyDivideModuloRightHandSide withOperator(hydra.ext.cypher.openCypher.MultiplyDivideModuloOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new MultiplyDivideModuloRightHandSide(operator, expression);
  }
  
  public MultiplyDivideModuloRightHandSide withExpression(hydra.ext.cypher.openCypher.PowerOfExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new MultiplyDivideModuloRightHandSide(operator, expression);
  }
}
