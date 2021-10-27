package hydra.core;

/**
 * @type string
 */
public class FieldName {
  public final String value;
  
  /**
   * Constructs an immutable FieldName object
   */
  public FieldName(String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldName)) {
        return false;
    }
    FieldName o = (FieldName) other;
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
