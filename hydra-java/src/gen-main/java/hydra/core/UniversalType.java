package hydra.core;

/**
 * A universally quantified ('forall') type, parameterized by a type variable
 */
public class UniversalType {
  public final hydra.core.TypeVariable variable;
  
  public final hydra.core.Type body;
  
  /**
   * Constructs an immutable UniversalType object
   */
  public UniversalType(hydra.core.TypeVariable variable, hydra.core.Type body) {
    this.variable = variable;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UniversalType)) {
        return false;
    }
    UniversalType o = (UniversalType) other;
    return variable.equals(o.variable)
        && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode()
        + 3 * body.hashCode();
  }
  
  /**
   * Construct a new immutable UniversalType object in which variable is overridden
   */
  public UniversalType withVariable(hydra.core.TypeVariable variable) {
    return new UniversalType(variable, body);
  }
  
  /**
   * Construct a new immutable UniversalType object in which body is overridden
   */
  public UniversalType withBody(hydra.core.Type body) {
    return new UniversalType(variable, body);
  }
}
