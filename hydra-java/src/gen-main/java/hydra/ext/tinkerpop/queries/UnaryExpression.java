// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.queries;

import java.io.Serializable;

public class UnaryExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/queries.UnaryExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_OPERAND = new hydra.core.Name("operand");
  
  public final hydra.ext.tinkerpop.queries.UnaryOperator operator;
  
  public final hydra.ext.tinkerpop.queries.Expression operand;
  
  public UnaryExpression (hydra.ext.tinkerpop.queries.UnaryOperator operator, hydra.ext.tinkerpop.queries.Expression operand) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((operand));
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
  
  public UnaryExpression withOperator(hydra.ext.tinkerpop.queries.UnaryOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new UnaryExpression(operator, operand);
  }
  
  public UnaryExpression withOperand(hydra.ext.tinkerpop.queries.Expression operand) {
    java.util.Objects.requireNonNull((operand));
    return new UnaryExpression(operator, operand);
  }
}
