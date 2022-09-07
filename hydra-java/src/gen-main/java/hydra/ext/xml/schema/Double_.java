package hydra.ext.xml.schema;

public class Double_ {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/xml/schema.Double");
  
  public final Double value;
  
  public Double_ (Double value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Double_)) {
      return false;
    }
    Double_ o = (Double_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}