// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An application expression
 */
public class ApplicationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ApplicationExpression");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENT = new hydra.core.Name("argument");
  
  public final hydra.ext.haskell.ast.Expression function;
  
  public final hydra.ext.haskell.ast.Expression argument;
  
  public ApplicationExpression (hydra.ext.haskell.ast.Expression function, hydra.ext.haskell.ast.Expression argument) {
    java.util.Objects.requireNonNull((function));
    java.util.Objects.requireNonNull((argument));
    this.function = function;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationExpression)) {
      return false;
    }
    ApplicationExpression o = (ApplicationExpression) (other);
    return function.equals(o.function) && argument.equals(o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * function.hashCode() + 3 * argument.hashCode();
  }
  
  public ApplicationExpression withFunction(hydra.ext.haskell.ast.Expression function) {
    java.util.Objects.requireNonNull((function));
    return new ApplicationExpression(function, argument);
  }
  
  public ApplicationExpression withArgument(hydra.ext.haskell.ast.Expression argument) {
    java.util.Objects.requireNonNull((argument));
    return new ApplicationExpression(function, argument);
  }
}