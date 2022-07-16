package hydra.ext.xml.schema;

public class DateTime {
  public final String value;
  
  public DateTime (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DateTime)) {
      return false;
    }
    DateTime o = (DateTime) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}