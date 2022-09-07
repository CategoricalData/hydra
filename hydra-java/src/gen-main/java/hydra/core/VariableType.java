package hydra.core;

/**
 * A symbol which stands in for a type
 */
public class VariableType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.VariableType");
  
  /**
   * A symbol which stands in for a type
   */
  public final String value;
  
  public VariableType (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableType)) {
      return false;
    }
    VariableType o = (VariableType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}