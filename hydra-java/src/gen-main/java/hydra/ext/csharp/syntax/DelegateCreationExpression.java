// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class DelegateCreationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.DelegateCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.csharp.syntax.DelegateType type;
  
  public final hydra.ext.csharp.syntax.Expression expression;
  
  public DelegateCreationExpression (hydra.ext.csharp.syntax.DelegateType type, hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((expression));
    this.type = type;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DelegateCreationExpression)) {
      return false;
    }
    DelegateCreationExpression o = (DelegateCreationExpression) (other);
    return type.equals(o.type) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * expression.hashCode();
  }
  
  public DelegateCreationExpression withType(hydra.ext.csharp.syntax.DelegateType type) {
    java.util.Objects.requireNonNull((type));
    return new DelegateCreationExpression(type, expression);
  }
  
  public DelegateCreationExpression withExpression(hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new DelegateCreationExpression(type, expression);
  }
}