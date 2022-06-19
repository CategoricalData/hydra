package hydra.core;

/**
 * A symbol which stands in for a type
 */
public class TypeVariable {
  /**
   * A symbol which stands in for a type
   */
  public final String value;
  
  public TypeVariable (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeVariable)) {
      return false;
    }
    TypeVariable o = (TypeVariable) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}