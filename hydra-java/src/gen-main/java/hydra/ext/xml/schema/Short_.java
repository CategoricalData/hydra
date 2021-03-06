package hydra.ext.xml.schema;

public class Short_ {
  public final Short value;
  
  public Short_ (Short value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Short_)) {
      return false;
    }
    Short_ o = (Short_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}