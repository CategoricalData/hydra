package hydra.core;

/**
 * A type application (instantiation), which applies a term to a type
 */
public class TypeApplication<M> {
  /**
   * A term which is the left-hand side of the application
   */
  public final Term<M> function;
  
  /**
   * A type which is the right-hand side of the application
   */
  public final Type<M> argument;
  
  public TypeApplication (Term<M> function, Type<M> argument) {
    this.function = function;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeApplication)) {
      return false;
    }
    TypeApplication o = (TypeApplication) (other);
    return function.equals(o.function) && argument.equals(o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * function.hashCode() + 3 * argument.hashCode();
  }
  
  public TypeApplication withFunction(Term<M> function) {
    return new TypeApplication(function, argument);
  }
  
  public TypeApplication withArgument(Type<M> argument) {
    return new TypeApplication(function, argument);
  }
}