// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class NullConditionalElementAccess implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NullConditionalElementAccess");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public static final hydra.core.Name FIELD_NAME_DEPENDENT_ACCESS = new hydra.core.Name("dependentAccess");
  
  public final hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression expression;
  
  public final hydra.ext.csharp.syntax.ArgumentList arguments;
  
  public final java.util.List<hydra.ext.csharp.syntax.DependentAccess> dependentAccess;
  
  public NullConditionalElementAccess (hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression expression, hydra.ext.csharp.syntax.ArgumentList arguments, java.util.List<hydra.ext.csharp.syntax.DependentAccess> dependentAccess) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((arguments));
    java.util.Objects.requireNonNull((dependentAccess));
    this.expression = expression;
    this.arguments = arguments;
    this.dependentAccess = dependentAccess;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NullConditionalElementAccess)) {
      return false;
    }
    NullConditionalElementAccess o = (NullConditionalElementAccess) (other);
    return expression.equals(o.expression) && arguments.equals(o.arguments) && dependentAccess.equals(o.dependentAccess);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * arguments.hashCode() + 5 * dependentAccess.hashCode();
  }
  
  public NullConditionalElementAccess withExpression(hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new NullConditionalElementAccess(expression, arguments, dependentAccess);
  }
  
  public NullConditionalElementAccess withArguments(hydra.ext.csharp.syntax.ArgumentList arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new NullConditionalElementAccess(expression, arguments, dependentAccess);
  }
  
  public NullConditionalElementAccess withDependentAccess(java.util.List<hydra.ext.csharp.syntax.DependentAccess> dependentAccess) {
    java.util.Objects.requireNonNull((dependentAccess));
    return new NullConditionalElementAccess(expression, arguments, dependentAccess);
  }
}