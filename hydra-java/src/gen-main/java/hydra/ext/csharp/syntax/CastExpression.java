// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class CastExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.CastExpression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.UnaryExpression expression;
  
  public CastExpression (hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.UnaryExpression expression) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((expression));
    this.type = type;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastExpression)) {
      return false;
    }
    CastExpression o = (CastExpression) (other);
    return type.equals(o.type) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * expression.hashCode();
  }
  
  public CastExpression withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new CastExpression(type, expression);
  }
  
  public CastExpression withExpression(hydra.ext.csharp.syntax.UnaryExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new CastExpression(type, expression);
  }
}