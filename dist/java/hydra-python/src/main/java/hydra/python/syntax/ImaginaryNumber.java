// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class ImaginaryNumber implements Serializable, Comparable<ImaginaryNumber> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.ImaginaryNumber");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.python.syntax.Number_ value;

  public ImaginaryNumber (hydra.python.syntax.Number_ value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImaginaryNumber)) {
      return false;
    }
    ImaginaryNumber o = (ImaginaryNumber) other;
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
  public int compareTo(ImaginaryNumber other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
