package hydra.langs.kusto.kql;

import java.io.Serializable;

public class UnaryExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.UnaryExpression");
  
  public final hydra.langs.kusto.kql.UnaryOperator operator;
  
  public final hydra.langs.kusto.kql.Expression expression;
  
  public UnaryExpression (hydra.langs.kusto.kql.UnaryOperator operator, hydra.langs.kusto.kql.Expression expression) {
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
  
  public UnaryExpression withOperator(hydra.langs.kusto.kql.UnaryOperator operator) {
    return new UnaryExpression(operator, expression);
  }
  
  public UnaryExpression withExpression(hydra.langs.kusto.kql.Expression expression) {
    return new UnaryExpression(operator, expression);
  }
}