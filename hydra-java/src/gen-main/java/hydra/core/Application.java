package hydra.core;

/**
 * A term which applies a function to an argument
 */
public class Application {
  public final hydra.core.Term function;
  
  public final hydra.core.Term argument;
  
  /**
   * Constructs an immutable Application object
   */
  public Application(hydra.core.Term function, hydra.core.Term argument) {
    this.function = function;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Application)) {
        return false;
    }
    Application o = (Application) other;
    return function.equals(o.function)
        && argument.equals(o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * function.hashCode()
        + 3 * argument.hashCode();
  }
  
  /**
   * Construct a new immutable Application object in which function is overridden
   */
  public Application withFunction(hydra.core.Term function) {
    return new Application(function, argument);
  }
  
  /**
   * Construct a new immutable Application object in which argument is overridden
   */
  public Application withArgument(hydra.core.Term argument) {
    return new Application(function, argument);
  }
}
