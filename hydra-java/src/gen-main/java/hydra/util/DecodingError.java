// Note: this is an automatically generated file. Do not edit.

package hydra.util;

import java.io.Serializable;

/**
 * An error that occurred during decoding of a term
 */
public class DecodingError implements Serializable, Comparable<DecodingError> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.util.DecodingError");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public DecodingError (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DecodingError)) {
      return false;
    }
    DecodingError o = (DecodingError) (other);
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
  public int compareTo(DecodingError other) {
    return ((Comparable) (value)).compareTo(other.value);
  }
}
