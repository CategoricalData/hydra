package hydra.ext.xml.schema;

public class AnyURI {
  public final String value;
  
  public AnyURI (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnyURI)) {
      return false;
    }
    AnyURI o = (AnyURI) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}