package hydra.langs.kusto.kql;

import java.io.Serializable;

public class PropertyExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.PropertyExpression");
  
  public final hydra.langs.kusto.kql.Expression expression;
  
  public final String property;
  
  public PropertyExpression (hydra.langs.kusto.kql.Expression expression, String property) {
    this.expression = expression;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyExpression)) {
      return false;
    }
    PropertyExpression o = (PropertyExpression) (other);
    return expression.equals(o.expression) && property.equals(o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * property.hashCode();
  }
  
  public PropertyExpression withExpression(hydra.langs.kusto.kql.Expression expression) {
    return new PropertyExpression(expression, property);
  }
  
  public PropertyExpression withProperty(String property) {
    return new PropertyExpression(expression, property);
  }
}