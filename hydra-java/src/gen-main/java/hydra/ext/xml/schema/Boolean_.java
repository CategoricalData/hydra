package hydra.ext.xml.schema;

public class Boolean_ {
  public final Boolean value;
  
  public Boolean_ (Boolean value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Boolean_)) {
      return false;
    }
    Boolean_ o = (Boolean_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}