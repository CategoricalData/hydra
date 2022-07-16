package hydra.ext.pegasus.pdl;

public class Package_ {
  public final String value;
  
  public Package_ (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Package_)) {
      return false;
    }
    Package_ o = (Package_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}