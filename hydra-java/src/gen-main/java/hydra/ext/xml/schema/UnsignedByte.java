package hydra.ext.xml.schema;

public class UnsignedByte {
  public final Byte value;
  
  public UnsignedByte (Byte value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnsignedByte)) {
      return false;
    }
    UnsignedByte o = (UnsignedByte) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}