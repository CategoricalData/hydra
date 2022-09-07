package hydra.core;

/**
 * A symbol which stands in for a term
 */
public class Variable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Variable");
  
  /**
   * A symbol which stands in for a term
   */
  public final String value;
  
  public Variable (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Variable)) {
      return false;
    }
    Variable o = (Variable) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}