// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class UnsignedByte implements Serializable, Comparable<UnsignedByte> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.UnsignedByte");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final Short value;
  
  public UnsignedByte (Short value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnsignedByte)) {
      return false;
    }
    UnsignedByte o = (UnsignedByte) other;
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
  public int compareTo(UnsignedByte other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
