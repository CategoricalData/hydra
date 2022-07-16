package hydra.ext.xml.schema;

public class UnsignedInt {
  public final Long value;
  
  public UnsignedInt (Long value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnsignedInt)) {
      return false;
    }
    UnsignedInt o = (UnsignedInt) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}