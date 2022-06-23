package hydra.core;

/**
 * A type abstraction; the type-level analog of a lambda term
 */
public class TypeLambda<M> {
  /**
   * The parameter of the lambda
   */
  public final TypeVariable parameter;
  
  /**
   * The body of the lambda
   */
  public final Type<M> body;
  
  public TypeLambda (TypeVariable parameter, Type<M> body) {
    this.parameter = parameter;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeLambda)) {
      return false;
    }
    TypeLambda o = (TypeLambda) (other);
    return parameter.equals(o.parameter) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode() + 3 * body.hashCode();
  }
  
  public TypeLambda withParameter(TypeVariable parameter) {
    return new TypeLambda(parameter, body);
  }
  
  public TypeLambda withBody(Type<M> body) {
    return new TypeLambda(parameter, body);
  }
}