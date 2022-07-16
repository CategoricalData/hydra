package hydra.ext.xml.schema;

public class GMonth {
  public final String value;
  
  public GMonth (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GMonth)) {
      return false;
    }
    GMonth o = (GMonth) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}