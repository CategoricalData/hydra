package hydra.core;

/**
 * A type application (instantiation), which applies a term to a type
 */
public class TypeApplication<A> {
  /**
   * A term which is the left-hand side of the application
   */
  public final hydra.core.Term<A> function;
  
  /**
   * A type which is the right-hand side of the application
   */
  public final hydra.core.Type argument;
  
  /**
   * Constructs an immutable TypeApplication object
   */
  public TypeApplication(hydra.core.Term<A> function, hydra.core.Type argument) {
    this.function = function;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeApplication)) {
        return false;
    }
    TypeApplication o = (TypeApplication) other;
    return function.equals(o.function)
        && argument.equals(o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * function.hashCode()
        + 3 * argument.hashCode();
  }
  
  /**
   * Construct a new immutable TypeApplication object in which function is overridden
   */
  public TypeApplication withFunction(hydra.core.Term<A> function) {
    return new TypeApplication(function, argument);
  }
  
  /**
   * Construct a new immutable TypeApplication object in which argument is overridden
   */
  public TypeApplication withArgument(hydra.core.Type argument) {
    return new TypeApplication(function, argument);
  }
}
