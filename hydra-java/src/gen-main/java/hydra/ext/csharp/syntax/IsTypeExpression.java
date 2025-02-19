// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class IsTypeExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.IsTypeExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.ext.csharp.syntax.RelationalExpression expression;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public IsTypeExpression (hydra.ext.csharp.syntax.RelationalExpression expression, hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((type));
    this.expression = expression;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IsTypeExpression)) {
      return false;
    }
    IsTypeExpression o = (IsTypeExpression) (other);
    return expression.equals(o.expression) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * type.hashCode();
  }
  
  public IsTypeExpression withExpression(hydra.ext.csharp.syntax.RelationalExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new IsTypeExpression(expression, type);
  }
  
  public IsTypeExpression withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new IsTypeExpression(expression, type);
  }
}