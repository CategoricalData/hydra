// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class RealNumber implements Serializable, Comparable<RealNumber> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.RealNumber");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.python.syntax.Number_ value;

  public RealNumber (hydra.python.syntax.Number_ value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RealNumber)) {
      return false;
    }
    RealNumber o = (RealNumber) other;
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
  public int compareTo(RealNumber other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
