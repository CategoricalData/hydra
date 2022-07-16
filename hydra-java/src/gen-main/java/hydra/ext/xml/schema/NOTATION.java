package hydra.ext.xml.schema;

public class NOTATION {
  public final String value;
  
  public NOTATION (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NOTATION)) {
      return false;
    }
    NOTATION o = (NOTATION) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}