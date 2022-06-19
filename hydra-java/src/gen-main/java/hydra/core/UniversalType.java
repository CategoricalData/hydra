package hydra.core;

/**
 * A universally quantified ('forall') type, parameterized by a type variable
 */
public class UniversalType<M> {
  public final TypeVariable variable;
  
  public final Type<M> body;
  
  public UniversalType (TypeVariable variable, Type<M> body) {
    this.variable = variable;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UniversalType)) {
      return false;
    }
    UniversalType o = (UniversalType) (other);
    return variable.equals(o.variable) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode() + 3 * body.hashCode();
  }
  
  public UniversalType withVariable(TypeVariable variable) {
    return new UniversalType(variable, body);
  }
  
  public UniversalType withBody(Type<M> body) {
    return new UniversalType(variable, body);
  }
}