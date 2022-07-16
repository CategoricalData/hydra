package hydra.core;

/**
 * A type abstraction; the type-level analog of a lambda term
 */
public class LambdaType<M> {
  /**
   * The parameter of the lambda
   */
  public final VariableType parameter;
  
  /**
   * The body of the lambda
   */
  public final Type<M> body;
  
  public LambdaType (VariableType parameter, Type<M> body) {
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
  
  public LambdaType withParameter(VariableType parameter) {
    return new LambdaType(parameter, body);
  }
  
  public LambdaType withBody(Type<M> body) {
    return new LambdaType(parameter, body);
  }
}