// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class Long_ implements Serializable, Comparable<Long_> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.Long");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final Long value;
  
  public Long_ (Long value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Long_)) {
      return false;
    }
    Long_ o = (Long_) other;
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
  public int compareTo(Long_ other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
