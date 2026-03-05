// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class Float_ implements Serializable, Comparable<Float_> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.Float");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final Float value;
  
  public Float_ (Float value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Float_)) {
      return false;
    }
    Float_ o = (Float_) other;
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
  public int compareTo(Float_ other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
