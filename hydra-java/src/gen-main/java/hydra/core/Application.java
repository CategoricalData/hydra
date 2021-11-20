package hydra.core;

/**
 * A term which applies a function to an argument
 */
public class Application<A> {
  public final hydra.core.Term<A> function;
  
  public final hydra.core.Term<A> argument;
  
  /**
   * Constructs an immutable Application object
   */
  public Application(hydra.core.Term<A> function, hydra.core.Term<A> argument) {
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
  public Application withFunction(hydra.core.Term<A> function) {
    return new Application(function, argument);
  }
  
  /**
   * Construct a new immutable Application object in which argument is overridden
   */
  public Application withArgument(hydra.core.Term<A> argument) {
    return new Application(function, argument);
  }
}
