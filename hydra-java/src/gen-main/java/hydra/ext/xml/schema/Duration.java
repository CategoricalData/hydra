package hydra.ext.xml.schema;

public class Duration {
  public final String value;
  
  public Duration (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Duration)) {
      return false;
    }
    Duration o = (Duration) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}