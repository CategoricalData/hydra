// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class PnLocalEsc implements Serializable, Comparable<PnLocalEsc> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocalEsc");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public PnLocalEsc (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnLocalEsc)) {
      return false;
    }
    PnLocalEsc o = (PnLocalEsc) other;
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
  public int compareTo(PnLocalEsc other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
