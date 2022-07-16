package hydra.ext.xml.schema;

public class IDREF {
  public final String value;
  
  public IDREF (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IDREF)) {
      return false;
    }
    IDREF o = (IDREF) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}