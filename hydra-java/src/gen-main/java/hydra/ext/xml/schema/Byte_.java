package hydra.ext.xml.schema;

public class Byte_ {
  public final Short value;
  
  public Byte_ (Short value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Byte_)) {
      return false;
    }
    Byte_ o = (Byte_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}