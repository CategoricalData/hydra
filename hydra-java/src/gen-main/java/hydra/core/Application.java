package hydra.core;

/**
 * A term which applies a function to an argument
 */
public class Application<M> {
  /**
   * The left-hand side of the application
   */
  public final hydra.core.Term<M> function;
  
  /**
   * The right-hand side of the application
   */
  public final hydra.core.Term<M> argument;
  
  public Application (hydra.core.Term<M> function, hydra.core.Term<M> argument) {
    this.function = function;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Application)) {
      return false;
    }
    Application o = (Application) (other);
    return function.equals(o.function) && argument.equals(o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * function.hashCode() + 3 * argument.hashCode();
  }
  
  public Application withFunction(hydra.core.Term<M> function) {
    return new Application(function, argument);
  }
  
  public Application withArgument(hydra.core.Term<M> argument) {
    return new Application(function, argument);
  }
}