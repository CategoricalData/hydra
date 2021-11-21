package hydra.core;

/**
 * A type abstraction (generalization), which binds a type variable to a term
 */
public class TypeAbstraction<A> {
  /**
   * The parameter of the abstraction
   */
  public final hydra.core.TypeVariable parameter;
  
  /**
   * The body of the abstraction
   */
  public final hydra.core.Term<A> body;
  
  /**
   * Constructs an immutable TypeAbstraction object
   */
  public TypeAbstraction(hydra.core.TypeVariable parameter, hydra.core.Term<A> body) {
    this.parameter = parameter;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeAbstraction)) {
        return false;
    }
    TypeAbstraction o = (TypeAbstraction) other;
    return parameter.equals(o.parameter)
        && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode()
        + 3 * body.hashCode();
  }
  
  /**
   * Construct a new immutable TypeAbstraction object in which parameter is overridden
   */
  public TypeAbstraction withParameter(hydra.core.TypeVariable parameter) {
    return new TypeAbstraction(parameter, body);
  }
  
  /**
   * Construct a new immutable TypeAbstraction object in which body is overridden
   */
  public TypeAbstraction withBody(hydra.core.Term<A> body) {
    return new TypeAbstraction(parameter, body);
  }
}
