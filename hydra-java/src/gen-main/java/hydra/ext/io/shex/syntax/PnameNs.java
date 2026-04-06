// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class PnameNs implements Serializable, Comparable<PnameNs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PnameNs");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.PnPrefix> value;

  public PnameNs (hydra.util.Maybe<hydra.ext.io.shex.syntax.PnPrefix> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnameNs)) {
      return false;
    }
    PnameNs o = (PnameNs) other;
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
  public int compareTo(PnameNs other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
