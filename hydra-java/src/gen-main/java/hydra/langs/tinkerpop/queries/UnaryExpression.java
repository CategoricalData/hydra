package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class UnaryExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.UnaryExpression");
  
  public final hydra.langs.tinkerpop.queries.UnaryOperator operator;
  
  public final hydra.langs.tinkerpop.queries.Expression operand;
  
  public UnaryExpression (hydra.langs.tinkerpop.queries.UnaryOperator operator, hydra.langs.tinkerpop.queries.Expression operand) {
    this.operator = operator;
    this.operand = operand;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnaryExpression)) {
      return false;
    }
    UnaryExpression o = (UnaryExpression) (other);
    return operator.equals(o.operator) && operand.equals(o.operand);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * operand.hashCode();
  }
  
  public UnaryExpression withOperator(hydra.langs.tinkerpop.queries.UnaryOperator operator) {
    return new UnaryExpression(operator, operand);
  }
  
  public UnaryExpression withOperand(hydra.langs.tinkerpop.queries.Expression operand) {
    return new UnaryExpression(operator, operand);
  }
}