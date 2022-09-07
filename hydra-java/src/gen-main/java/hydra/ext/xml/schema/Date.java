package hydra.ext.xml.schema;

public class Date {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/xml/schema.Date");
  
  public final String value;
  
  public Date (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Date)) {
      return false;
    }
    Date o = (Date) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}