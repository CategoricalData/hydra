// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class MultiplyDivideModuloRightHandSide implements Serializable, Comparable<MultiplyDivideModuloRightHandSide> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.MultiplyDivideModuloRightHandSide");
  
  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.cypher.openCypher.MultiplyDivideModuloOperator operator;
  
  public final hydra.ext.cypher.openCypher.PowerOfExpression expression;
  
  public MultiplyDivideModuloRightHandSide (hydra.ext.cypher.openCypher.MultiplyDivideModuloOperator operator, hydra.ext.cypher.openCypher.PowerOfExpression expression) {
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiplyDivideModuloRightHandSide)) {
      return false;
    }
    MultiplyDivideModuloRightHandSide o = (MultiplyDivideModuloRightHandSide) other;
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
  public int compareTo(MultiplyDivideModuloRightHandSide other) {
    int cmp = 0;
    cmp = ((Comparable) operator).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expression).compareTo(other.expression);
  }
  
  public MultiplyDivideModuloRightHandSide withOperator(hydra.ext.cypher.openCypher.MultiplyDivideModuloOperator operator) {
    return new MultiplyDivideModuloRightHandSide(operator, expression);
  }
  
  public MultiplyDivideModuloRightHandSide withExpression(hydra.ext.cypher.openCypher.PowerOfExpression expression) {
    return new MultiplyDivideModuloRightHandSide(operator, expression);
  }
}
