package hydra.ext.xml.schema;

public class AnySimpleType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/xml/schema.AnySimpleType");
  
  public final String value;
  
  public AnySimpleType (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnySimpleType)) {
      return false;
    }
    AnySimpleType o = (AnySimpleType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}