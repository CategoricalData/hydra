// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class UnaryExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.query.UnaryExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_OPERAND = new hydra.core.Name("operand");
  
  public final hydra.pg.query.UnaryOperator operator;
  
  public final hydra.pg.query.Expression operand;
  
  public UnaryExpression (hydra.pg.query.UnaryOperator operator, hydra.pg.query.Expression operand) {
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
  
  public UnaryExpression withOperator(hydra.pg.query.UnaryOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new UnaryExpression(operator, operand);
  }
  
  public UnaryExpression withOperand(hydra.pg.query.Expression operand) {
    java.util.Objects.requireNonNull((operand));
    return new UnaryExpression(operator, operand);
  }
}
