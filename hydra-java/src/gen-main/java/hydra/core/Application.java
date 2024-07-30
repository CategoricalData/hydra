// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term which applies a function to an argument
 */
public class Application implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Application");
  
  /**
   * The left-hand side of the application
   */
  public final hydra.core.Term function;
  
  /**
   * The right-hand side of the application
   */
  public final hydra.core.Term argument;
  
  public Application (hydra.core.Term function, hydra.core.Term argument) {
    java.util.Objects.requireNonNull((function));
    java.util.Objects.requireNonNull((argument));
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
  
  public Application withFunction(hydra.core.Term function) {
    java.util.Objects.requireNonNull((function));
    return new Application(function, argument);
  }
  
  public Application withArgument(hydra.core.Term argument) {
    java.util.Objects.requireNonNull((argument));
    return new Application(function, argument);
  }
}