package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class MultiplyDivideModuloRightHandSide implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MultiplyDivideModuloRightHandSide");
  
  public final hydra.langs.cypher.openCypher.MultiplyDivideModuloOperator operator;
  
  public final hydra.langs.cypher.openCypher.PowerOfExpression expression;
  
  public MultiplyDivideModuloRightHandSide (hydra.langs.cypher.openCypher.MultiplyDivideModuloOperator operator, hydra.langs.cypher.openCypher.PowerOfExpression expression) {
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
  
  public MultiplyDivideModuloRightHandSide withOperator(hydra.langs.cypher.openCypher.MultiplyDivideModuloOperator operator) {
    return new MultiplyDivideModuloRightHandSide(operator, expression);
  }
  
  public MultiplyDivideModuloRightHandSide withExpression(hydra.langs.cypher.openCypher.PowerOfExpression expression) {
    return new MultiplyDivideModuloRightHandSide(operator, expression);
  }
}