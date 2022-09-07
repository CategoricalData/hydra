package hydra.ext.xml.schema;

public class String_ {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/xml/schema.String");
  
  public final String value;
  
  public String_ (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof String_)) {
      return false;
    }
    String_ o = (String_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}