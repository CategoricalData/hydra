package hydra.ext.xml.schema;

public class ENTITY {
  public final String value;
  
  public ENTITY (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ENTITY)) {
      return false;
    }
    ENTITY o = (ENTITY) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}