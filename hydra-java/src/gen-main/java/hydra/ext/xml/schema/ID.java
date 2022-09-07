package hydra.ext.xml.schema;

public class ID {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/xml/schema.ID");
  
  public final String value;
  
  public ID (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ID)) {
      return false;
    }
    ID o = (ID) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}