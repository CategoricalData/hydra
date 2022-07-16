package hydra.ext.java.syntax;

/**
 * Note: this is an approximation which ignores encoding
 */
public class StringLiteral {
  /**
   * Note: this is an approximation which ignores encoding
   */
  public final String value;
  
  public StringLiteral (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteral)) {
      return false;
    }
    StringLiteral o = (StringLiteral) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}