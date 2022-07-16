package hydra.ext.xml.schema;

public class UnsignedLong {
  public final java.math.BigInteger value;
  
  public UnsignedLong (java.math.BigInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnsignedLong)) {
      return false;
    }
    UnsignedLong o = (UnsignedLong) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}