package hydra.langs.xml.schema;

public class Decimal {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/xml/schema.Decimal");
  
  public final String value;
  
  public Decimal (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Decimal)) {
      return false;
    }
    Decimal o = (Decimal) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}