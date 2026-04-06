// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class AtpNameNs implements Serializable, Comparable<AtpNameNs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.AtpNameNs");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.PnPrefix> value;

  public AtpNameNs (hydra.util.Maybe<hydra.ext.io.shex.syntax.PnPrefix> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtpNameNs)) {
      return false;
    }
    AtpNameNs o = (AtpNameNs) other;
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
  public int compareTo(AtpNameNs other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
