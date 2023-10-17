package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class AddOrSubtractRhs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.AddOrSubtractRhs");
  
  public final hydra.langs.cypher.openCypher.PlusOrMinus operator;
  
  public final hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression expression;
  
  public AddOrSubtractRhs (hydra.langs.cypher.openCypher.PlusOrMinus operator, hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression expression) {
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AddOrSubtractRhs)) {
      return false;
    }
    AddOrSubtractRhs o = (AddOrSubtractRhs) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public AddOrSubtractRhs withOperator(hydra.langs.cypher.openCypher.PlusOrMinus operator) {
    return new AddOrSubtractRhs(operator, expression);
  }
  
  public AddOrSubtractRhs withExpression(hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression expression) {
    return new AddOrSubtractRhs(operator, expression);
  }
}