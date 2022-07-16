package hydra.ext.xml.schema;

public class PositiveInteger {
  public final java.math.BigInteger value;
  
  public PositiveInteger (java.math.BigInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PositiveInteger)) {
      return false;
    }
    PositiveInteger o = (PositiveInteger) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}