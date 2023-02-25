package hydra.core;

/**
 * A type abstraction; the type-level analog of a lambda term
 */
public class LambdaType<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.LambdaType");
  
  /**
   * The parameter of the lambda
   */
  public final hydra.core.Name parameter;
  
  /**
   * The body of the lambda
   */
  public final hydra.core.Type<M> body;
  
  public LambdaType (hydra.core.Name parameter, hydra.core.Type<M> body) {
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
    return new LambdaType(parameter, body);
  }
  
  public LambdaType withBody(hydra.core.Type<M> body) {
    return new LambdaType(parameter, body);
  }
}