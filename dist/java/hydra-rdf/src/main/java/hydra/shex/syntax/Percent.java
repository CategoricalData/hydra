// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class Percent implements Serializable, Comparable<Percent> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.Percent");

  public static final hydra.core.Name HEX = new hydra.core.Name("Hex");

  public static final hydra.core.Name HEX2 = new hydra.core.Name("Hex2");

  public final hydra.shex.syntax.Hex Hex;

  public final hydra.shex.syntax.Hex Hex2;

  public Percent (hydra.shex.syntax.Hex Hex, hydra.shex.syntax.Hex Hex2) {
    this.Hex = Hex;
    this.Hex2 = Hex2;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Percent)) {
      return false;
    }
    Percent o = (Percent) other;
    return java.util.Objects.equals(
      this.Hex,
      o.Hex) && java.util.Objects.equals(
      this.Hex2,
      o.Hex2);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Hex) + 3 * java.util.Objects.hashCode(Hex2);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Percent other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      Hex,
      other.Hex);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      Hex2,
      other.Hex2);
  }

  public Percent withHex(hydra.shex.syntax.Hex Hex) {
    return new Percent(Hex, Hex2);
  }

  public Percent withHex2(hydra.shex.syntax.Hex Hex2) {
    return new Percent(Hex, Hex2);
  }
}
