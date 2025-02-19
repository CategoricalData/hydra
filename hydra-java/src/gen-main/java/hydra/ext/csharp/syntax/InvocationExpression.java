// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class InvocationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InvocationExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.csharp.syntax.PrimaryExpression expression;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> arguments;
  
  public InvocationExpression (hydra.ext.csharp.syntax.PrimaryExpression expression, hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> arguments) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((arguments));
    this.expression = expression;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InvocationExpression)) {
      return false;
    }
    InvocationExpression o = (InvocationExpression) (other);
    return expression.equals(o.expression) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * arguments.hashCode();
  }
  
  public InvocationExpression withExpression(hydra.ext.csharp.syntax.PrimaryExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new InvocationExpression(expression, arguments);
  }
  
  public InvocationExpression withArguments(hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new InvocationExpression(expression, arguments);
  }
}