// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class Default implements Serializable, Comparable<Default> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.Default");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.python.syntax.Expression value;

  public Default (hydra.python.syntax.Expression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Default)) {
      return false;
    }
    Default o = (Default) other;
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
  public int compareTo(Default other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
