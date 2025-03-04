// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public class PropertyExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.kusto.kql.PropertyExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public final hydra.ext.com.microsoft.kusto.kql.Expression expression;
  
  public final String property;
  
  public PropertyExpression (hydra.ext.com.microsoft.kusto.kql.Expression expression, String property) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((property));
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
  
  public PropertyExpression withExpression(hydra.ext.com.microsoft.kusto.kql.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new PropertyExpression(expression, property);
  }
  
  public PropertyExpression withProperty(String property) {
    java.util.Objects.requireNonNull((property));
    return new PropertyExpression(expression, property);
  }
}