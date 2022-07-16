package hydra.ext.xml.schema;

public class Time {
  public final String value;
  
  public Time (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Time)) {
      return false;
    }
    Time o = (Time) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}