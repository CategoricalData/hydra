package hydra.ext.xml.schema;

public class GYear {
  public final String value;
  
  public GYear (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GYear)) {
      return false;
    }
    GYear o = (GYear) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}