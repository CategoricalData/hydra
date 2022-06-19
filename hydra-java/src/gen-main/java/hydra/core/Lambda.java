package hydra.core;

/**
 * A function abstraction (lambda)
 */
public class Lambda<M> {
  /**
   * The parameter of the lambda
   */
  public final Variable parameter;
  
  /**
   * The body of the lambda
   */
  public final Term<M> body;
  
  public Lambda (Variable parameter, Term<M> body) {
    this.parameter = parameter;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Lambda)) {
      return false;
    }
    Lambda o = (Lambda) (other);
    return parameter.equals(o.parameter) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode() + 3 * body.hashCode();
  }
  
  public Lambda withParameter(Variable parameter) {
    return new Lambda(parameter, body);
  }
  
  public Lambda withBody(Term<M> body) {
    return new Lambda(parameter, body);
  }
}