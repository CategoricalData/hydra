// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term which applies a function to an argument
 */
public class Application implements Serializable, Comparable<Application> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Application");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENT = new hydra.core.Name("argument");
  
  /**
   * The left-hand side of the application
   */
  public final hydra.core.Term function;
  
  /**
   * The right-hand side of the application
   */
  public final hydra.core.Term argument;
  
  public Application (hydra.core.Term function, hydra.core.Term argument) {
    this.function = function;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Application)) {
      return false;
    }
    Application o = (Application) other;
    return java.util.Objects.equals(
      this.function,
      o.function) && java.util.Objects.equals(
      this.argument,
      o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(function) + 3 * java.util.Objects.hashCode(argument);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Application other) {
    int cmp = 0;
    cmp = ((Comparable) function).compareTo(other.function);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) argument).compareTo(other.argument);
  }
  
  public Application withFunction(hydra.core.Term function) {
    return new Application(function, argument);
  }
  
  public Application withArgument(hydra.core.Term argument) {
    return new Application(function, argument);
  }
}
