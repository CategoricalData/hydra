package hydra.core;

/**
 * The type-level analog of an application term
 */
public class ApplicationType<M> {
  /**
   * The left-hand side of the application
   */
  public final Type<M> function;
  
  /**
   * The right-hand side of the application
   */
  public final Type<M> argument;
  
  public ApplicationType (Type<M> function, Type<M> argument) {
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
  
  public ApplicationType withFunction(Type<M> function) {
    return new ApplicationType(function, argument);
  }
  
  public ApplicationType withArgument(Type<M> argument) {
    return new ApplicationType(function, argument);
  }
}