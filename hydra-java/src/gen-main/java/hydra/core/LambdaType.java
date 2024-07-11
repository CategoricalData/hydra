// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type abstraction; the type-level analog of a lambda term
 */
public class LambdaType<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.LambdaType");
  
  /**
   * The variable which is bound by the lambda
   */
  public final hydra.core.Name parameter;
  
  /**
   * The body of the lambda
   */
  public final hydra.core.Type<A> body;
  
  public LambdaType (hydra.core.Name parameter, hydra.core.Type<A> body) {
    java.util.Objects.requireNonNull((parameter));
    java.util.Objects.requireNonNull((body));
    this.parameter = parameter;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaType)) {
      return false;
    }
    LambdaType o = (LambdaType) (other);
    return parameter.equals(o.parameter) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode() + 3 * body.hashCode();
  }
  
  public LambdaType withParameter(hydra.core.Name parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new LambdaType(parameter, body);
  }
  
  public LambdaType withBody(hydra.core.Type<A> body) {
    java.util.Objects.requireNonNull((body));
    return new LambdaType(parameter, body);
  }
}