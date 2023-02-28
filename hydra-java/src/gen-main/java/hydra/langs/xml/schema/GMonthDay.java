package hydra.langs.xml.schema;

public class GMonthDay {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/xml/schema.GMonthDay");
  
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