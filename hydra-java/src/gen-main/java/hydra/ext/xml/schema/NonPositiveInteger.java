package hydra.ext.xml.schema;

public class NonPositiveInteger {
  public final java.math.BigInteger value;
  
  public NonPositiveInteger (java.math.BigInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonPositiveInteger)) {
      return false;
    }
    NonPositiveInteger o = (NonPositiveInteger) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}