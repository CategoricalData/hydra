package hydra.ext.xml.schema;

public class NMTOKEN {
  public final String value;
  
  public NMTOKEN (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NMTOKEN)) {
      return false;
    }
    NMTOKEN o = (NMTOKEN) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}