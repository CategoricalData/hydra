package hydra.ext.xml.schema;

public class IDREFS {
  public final String value;
  
  public IDREFS (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IDREFS)) {
      return false;
    }
    IDREFS o = (IDREFS) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}