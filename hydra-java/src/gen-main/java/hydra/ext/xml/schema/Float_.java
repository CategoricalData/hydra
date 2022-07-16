package hydra.ext.xml.schema;

public class Float_ {
  public final Float value;
  
  public Float_ (Float value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Float_)) {
      return false;
    }
    Float_ o = (Float_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}