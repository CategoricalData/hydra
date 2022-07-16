package hydra.ext.xml.schema;

public class Double_ {
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