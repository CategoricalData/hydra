package hydra.ext.haskell.ast;

/**
 * An application expression
 */
public class Expression_Application {
  public final Expression function;
  
  public final Expression argument;
  
  public Expression_Application (Expression function, Expression argument) {
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
  
  public Expression_Application withFunction(Expression function) {
    return new Expression_Application(function, argument);
  }
  
  public Expression_Application withArgument(Expression argument) {
    return new Expression_Application(function, argument);
  }
}