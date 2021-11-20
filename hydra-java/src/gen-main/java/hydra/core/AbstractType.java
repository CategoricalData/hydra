package hydra.core;

/**
 * A universally quantified ('forall') type, parameterized by a type variable
 */
public class AbstractType {
  public final hydra.core.TypeVariable variable;
  
  public final hydra.core.Type body;
  
  /**
   * Constructs an immutable AbstractType object
   */
  public AbstractType(hydra.core.TypeVariable variable, hydra.core.Type body) {
    this.variable = variable;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AbstractType)) {
        return false;
    }
    AbstractType o = (AbstractType) other;
    return variable.equals(o.variable)
        && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode()
        + 3 * body.hashCode();
  }
  
  /**
   * Construct a new immutable AbstractType object in which variable is overridden
   */
  public AbstractType withVariable(hydra.core.TypeVariable variable) {
    return new AbstractType(variable, body);
  }
  
  /**
   * Construct a new immutable AbstractType object in which body is overridden
   */
  public AbstractType withBody(hydra.core.Type body) {
    return new AbstractType(variable, body);
  }
}
