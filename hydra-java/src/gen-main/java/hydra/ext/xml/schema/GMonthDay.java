package hydra.ext.xml.schema;

public class GMonthDay {
  public final String value;
  
  public GMonthDay (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GMonthDay)) {
      return false;
    }
    GMonthDay o = (GMonthDay) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}