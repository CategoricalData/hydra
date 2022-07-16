package hydra.ext.java.syntax;

/**
 * Note: this is an approximation which ignores encoding
 */
public class IntegerLiteral {
  /**
   * Note: this is an approximation which ignores encoding
   */
  public final java.math.BigInteger value;
  
  public IntegerLiteral (java.math.BigInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IntegerLiteral)) {
      return false;
    }
    IntegerLiteral o = (IntegerLiteral) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}