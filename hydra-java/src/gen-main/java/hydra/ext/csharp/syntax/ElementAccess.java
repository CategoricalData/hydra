// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ElementAccess implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ElementAccess");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression expression;
  
  public final hydra.ext.csharp.syntax.ArgumentList arguments;
  
  public ElementAccess (hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression expression, hydra.ext.csharp.syntax.ArgumentList arguments) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((arguments));
    this.expression = expression;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementAccess)) {
      return false;
    }
    ElementAccess o = (ElementAccess) (other);
    return expression.equals(o.expression) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * arguments.hashCode();
  }
  
  public ElementAccess withExpression(hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new ElementAccess(expression, arguments);
  }
  
  public ElementAccess withArguments(hydra.ext.csharp.syntax.ArgumentList arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new ElementAccess(expression, arguments);
  }
}