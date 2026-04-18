// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class Hex implements Serializable, Comparable<Hex> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.Hex");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public Hex (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Hex)) {
      return false;
    }
    Hex o = (Hex) other;
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
  public int compareTo(Hex other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
