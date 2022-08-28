package hydra.core;

/**
 * The type-level analog of an application term
 */
public class ApplicationType<M> {
  /**
   * The left-hand side of the application
   */
  public final hydra.core.Type<M> function;
  
  /**
   * The right-hand side of the application
   */
  public final hydra.core.Type<M> argument;
  
  public ApplicationType (hydra.core.Type<M> function, hydra.core.Type<M> argument) {
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
  
  public ApplicationType withFunction(hydra.core.Type<M> function) {
    return new ApplicationType(function, argument);
  }
  
  public ApplicationType withArgument(hydra.core.Type<M> argument) {
    return new ApplicationType(function, argument);
  }
}