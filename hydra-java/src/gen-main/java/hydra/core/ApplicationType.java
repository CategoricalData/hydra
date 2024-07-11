// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * The type-level analog of an application term
 */
public class ApplicationType<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.ApplicationType");
  
  /**
   * The left-hand side of the application
   */
  public final hydra.core.Type<A> function;
  
  /**
   * The right-hand side of the application
   */
  public final hydra.core.Type<A> argument;
  
  public ApplicationType (hydra.core.Type<A> function, hydra.core.Type<A> argument) {
    java.util.Objects.requireNonNull((function));
    java.util.Objects.requireNonNull((argument));
    this.function = function;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationType)) {
      return false;
    }
    ApplicationType o = (ApplicationType) (other);
    return function.equals(o.function) && argument.equals(o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * function.hashCode() + 3 * argument.hashCode();
  }
  
  public ApplicationType withFunction(hydra.core.Type<A> function) {
    java.util.Objects.requireNonNull((function));
    return new ApplicationType(function, argument);
  }
  
  public ApplicationType withArgument(hydra.core.Type<A> argument) {
    java.util.Objects.requireNonNull((argument));
    return new ApplicationType(function, argument);
  }
}