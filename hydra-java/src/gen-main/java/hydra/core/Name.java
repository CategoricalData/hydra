package hydra.core;

/**
 * @type string
 */
public class Name {
  public final String value;
  
  /**
   * Constructs an immutable Name object
   */
  public Name(String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Name)) {
        return false;
    }
    Name o = (Name) other;
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
