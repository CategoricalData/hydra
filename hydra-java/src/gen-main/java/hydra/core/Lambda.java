package hydra.core;

/**
 * A function abstraction (lambda)
 */
public class Lambda {
  /**
   * The parameter of the lambda
   */
  public final Variable parameter;
  
  /**
   * The body of the lambda
   */
  public final Term body;
  
  /**
   * Constructs an immutable Lambda object
   */
  public Lambda(Variable parameter, Term body) {
    this.parameter = parameter;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Lambda)) {
        return false;
    }
    Lambda o = (Lambda) other;
    return parameter.equals(o.parameter)
        && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode()
        + 3 * body.hashCode();
  }
  
  /**
   * Construct a new immutable Lambda object in which parameter is overridden
   */
  public Lambda withParameter(Variable parameter) {
    return new Lambda(parameter, body);
  }
  
  /**
   * Construct a new immutable Lambda object in which body is overridden
   */
  public Lambda withBody(Term body) {
    return new Lambda(parameter, body);
  }
}
