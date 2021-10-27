package hydra.core;

/**
 * A function abstraction (lambda)
 */
public class Lambda {
  /**
   * The parameter of the lambda
   */
  public final hydra.core.Variable parameter;
  
  /**
   * The body of the lambda
   */
  public final hydra.core.Term body;
  
  /**
   * Constructs an immutable Lambda object
   */
  public Lambda(hydra.core.Variable parameter, hydra.core.Term body) {
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
  public Lambda withParameter(hydra.core.Variable parameter) {
    return new Lambda(parameter, body);
  }
  
  /**
   * Construct a new immutable Lambda object in which body is overridden
   */
  public Lambda withBody(hydra.core.Term body) {
    return new Lambda(parameter, body);
  }
}
