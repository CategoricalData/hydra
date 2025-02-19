// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class AsTypeExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AsTypeExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.ext.csharp.syntax.RelationalExpression expression;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public AsTypeExpression (hydra.ext.csharp.syntax.RelationalExpression expression, hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((type));
    this.expression = expression;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AsTypeExpression)) {
      return false;
    }
    AsTypeExpression o = (AsTypeExpression) (other);
    return expression.equals(o.expression) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * type.hashCode();
  }
  
  public AsTypeExpression withExpression(hydra.ext.csharp.syntax.RelationalExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new AsTypeExpression(expression, type);
  }
  
  public AsTypeExpression withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new AsTypeExpression(expression, type);
  }
}