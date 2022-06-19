package hydra.core;

/**
 * A unique element name
 */
public class Name {
  /**
   * A unique element name
   */
  public final String value;
  
  public Name (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Name)) {
      return false;
    }
    Name o = (Name) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}