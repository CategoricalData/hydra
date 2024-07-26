// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A function abstraction (lambda)
 */
public class Lambda implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Lambda");
  
  /**
   * The parameter of the lambda
   */
  public final hydra.core.Name parameter;
  
  /**
   * The body of the lambda
   */
  public final hydra.core.Term body;
  
  public Lambda (hydra.core.Name parameter, hydra.core.Term body) {
    java.util.Objects.requireNonNull((parameter));
    java.util.Objects.requireNonNull((body));
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
    java.util.Objects.requireNonNull((parameter));
    return new Lambda(parameter, body);
  }
  
  public Lambda withBody(hydra.core.Term body) {
    java.util.Objects.requireNonNull((body));
    return new Lambda(parameter, body);
  }
}