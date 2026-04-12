// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class Int implements Serializable, Comparable<Int> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.Int");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final Integer value;

  public Int (Integer value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Int)) {
      return false;
    }
    Int o = (Int) other;
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
  public int compareTo(Int other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
