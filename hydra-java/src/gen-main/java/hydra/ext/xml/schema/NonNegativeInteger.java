package hydra.ext.xml.schema;

public class NonNegativeInteger {
  public final java.math.BigInteger value;
  
  public NonNegativeInteger (java.math.BigInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonNegativeInteger)) {
      return false;
    }
    NonNegativeInteger o = (NonNegativeInteger) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}