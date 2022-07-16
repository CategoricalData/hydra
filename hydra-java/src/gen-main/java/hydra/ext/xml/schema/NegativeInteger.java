package hydra.ext.xml.schema;

public class NegativeInteger {
  public final java.math.BigInteger value;
  
  public NegativeInteger (java.math.BigInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NegativeInteger)) {
      return false;
    }
    NegativeInteger o = (NegativeInteger) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}