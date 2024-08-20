// Note: this is an automatically generated file. Do not edit.

package hydra.ext.kusto.kql;

import java.io.Serializable;

public class UnaryExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/kusto/kql.UnaryExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.kusto.kql.UnaryOperator operator;
  
  public final hydra.ext.kusto.kql.Expression expression;
  
  public UnaryExpression (hydra.ext.kusto.kql.UnaryOperator operator, hydra.ext.kusto.kql.Expression expression) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((expression));
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnaryExpression)) {
      return false;
    }
    UnaryExpression o = (UnaryExpression) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public UnaryExpression withOperator(hydra.ext.kusto.kql.UnaryOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new UnaryExpression(operator, expression);
  }
  
  public UnaryExpression withExpression(hydra.ext.kusto.kql.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new UnaryExpression(operator, expression);
  }
}