package hydra.ext.haskell.ast;

/**
 * An application expression
 */
public class Expression_Application {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Expression.Application");
  
  public final hydra.ext.haskell.ast.Expression function;
  
  public final hydra.ext.haskell.ast.Expression argument;
  
  public Expression_Application (hydra.ext.haskell.ast.Expression function, hydra.ext.haskell.ast.Expression argument) {
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
  
  public Expression_Application withFunction(hydra.ext.haskell.ast.Expression function) {
    return new Expression_Application(function, argument);
  }
  
  public Expression_Application withArgument(hydra.ext.haskell.ast.Expression argument) {
    return new Expression_Application(function, argument);
  }
}