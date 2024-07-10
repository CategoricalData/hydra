// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A function abstraction (lambda)
 */
public class Lambda<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Lambda");
  
  /**
   * The parameter of the lambda
   */
  public final hydra.core.Name parameter;
  
  /**
   * The body of the lambda
   */
  public final hydra.core.Term<A> body;
  
  public Lambda (hydra.core.Name parameter, hydra.core.Term<A> body) {
    if (parameter == null) {
      throw new IllegalArgumentException("null value for 'parameter' argument");
    }
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
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
  
  public Lambda withParameter(hydra.core.Name parameter) {
    if (parameter == null) {
      throw new IllegalArgumentException("null value for 'parameter' argument");
    }
    return new Lambda(parameter, body);
  }
  
  public Lambda withBody(hydra.core.Term<A> body) {
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
    return new Lambda(parameter, body);
  }
}