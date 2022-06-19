package hydra.core;

/**
 * A type abstraction (generalization), which binds a type variable to a term
 */
public class TypeAbstraction<M> {
  /**
   * The parameter of the abstraction
   */
  public final TypeVariable parameter;
  
  /**
   * The body of the abstraction
   */
  public final Term<M> body;
  
  public TypeAbstraction (TypeVariable parameter, Term<M> body) {
    this.parameter = parameter;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeAbstraction)) {
      return false;
    }
    TypeAbstraction o = (TypeAbstraction) (other);
    return parameter.equals(o.parameter) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode() + 3 * body.hashCode();
  }
  
  public TypeAbstraction withParameter(TypeVariable parameter) {
    return new TypeAbstraction(parameter, body);
  }
  
  public TypeAbstraction withBody(Term<M> body) {
    return new TypeAbstraction(parameter, body);
  }
}