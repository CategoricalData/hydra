// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

/**
 * An application expression
 */
public class Expression_Application implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/haskell/ast.Expression.Application");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENT = new hydra.core.Name("argument");
  
  public final hydra.langs.haskell.ast.Expression function;
  
  public final hydra.langs.haskell.ast.Expression argument;
  
  public Expression_Application (hydra.langs.haskell.ast.Expression function, hydra.langs.haskell.ast.Expression argument) {
    java.util.Objects.requireNonNull((function));
    java.util.Objects.requireNonNull((argument));
    this.function = function;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_Application)) {
      return false;
    }
    Expression_Application o = (Expression_Application) (other);
    return function.equals(o.function) && argument.equals(o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * function.hashCode() + 3 * argument.hashCode();
  }
  
  public Expression_Application withFunction(hydra.langs.haskell.ast.Expression function) {
    java.util.Objects.requireNonNull((function));
    return new Expression_Application(function, argument);
  }
  
  public Expression_Application withArgument(hydra.langs.haskell.ast.Expression argument) {
    java.util.Objects.requireNonNull((argument));
    return new Expression_Application(function, argument);
  }
}