package hydra.ext.coq.syntax;

/**
 * A non-negative arbitrary-precision integer
 */
public class Natural {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Natural");
  
  /**
   * A non-negative arbitrary-precision integer
   */
  public final java.math.BigInteger value;
  
  public Natural (java.math.BigInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Natural)) {
      return false;
    }
    Natural o = (Natural) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}