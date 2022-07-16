package hydra.ext.xml.schema;

public class Int {
  public final Integer value;
  
  public Int (Integer value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Int)) {
      return false;
    }
    Int o = (Int) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}