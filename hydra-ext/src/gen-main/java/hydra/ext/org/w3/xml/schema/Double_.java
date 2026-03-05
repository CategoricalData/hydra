// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class Double_ implements Serializable, Comparable<Double_> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.Double");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final Double value;
  
  public Double_ (Double value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Double_)) {
      return false;
    }
    Double_ o = (Double_) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Double_ other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
