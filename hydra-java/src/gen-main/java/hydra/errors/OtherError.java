// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * Any other error
 */
public class OtherError implements Serializable, Comparable<OtherError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.OtherError");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public OtherError (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OtherError)) {
      return false;
    }
    OtherError o = (OtherError) other;
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
  public int compareTo(OtherError other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
