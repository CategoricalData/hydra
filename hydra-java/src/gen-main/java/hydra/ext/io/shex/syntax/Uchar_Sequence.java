// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class Uchar_Sequence implements Serializable, Comparable<Uchar_Sequence> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar_Sequence");

  public static final hydra.core.Name HEX = new hydra.core.Name("Hex");

  public static final hydra.core.Name HEX2 = new hydra.core.Name("Hex2");

  public static final hydra.core.Name HEX3 = new hydra.core.Name("Hex3");

  public static final hydra.core.Name HEX4 = new hydra.core.Name("Hex4");

  public final hydra.ext.io.shex.syntax.Hex Hex;

  public final hydra.ext.io.shex.syntax.Hex Hex2;

  public final hydra.ext.io.shex.syntax.Hex Hex3;

  public final hydra.ext.io.shex.syntax.Hex Hex4;

  public Uchar_Sequence (hydra.ext.io.shex.syntax.Hex Hex, hydra.ext.io.shex.syntax.Hex Hex2, hydra.ext.io.shex.syntax.Hex Hex3, hydra.ext.io.shex.syntax.Hex Hex4) {
    this.Hex = Hex;
    this.Hex2 = Hex2;
    this.Hex3 = Hex3;
    this.Hex4 = Hex4;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Uchar_Sequence)) {
      return false;
    }
    Uchar_Sequence o = (Uchar_Sequence) other;
    return java.util.Objects.equals(
      this.Hex,
      o.Hex) && java.util.Objects.equals(
      this.Hex2,
      o.Hex2) && java.util.Objects.equals(
      this.Hex3,
      o.Hex3) && java.util.Objects.equals(
      this.Hex4,
      o.Hex4);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Hex) + 3 * java.util.Objects.hashCode(Hex2) + 5 * java.util.Objects.hashCode(Hex3) + 7 * java.util.Objects.hashCode(Hex4);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Uchar_Sequence other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      Hex,
      other.Hex);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      Hex2,
      other.Hex2);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      Hex3,
      other.Hex3);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      Hex4,
      other.Hex4);
  }

  public Uchar_Sequence withHex(hydra.ext.io.shex.syntax.Hex Hex) {
    return new Uchar_Sequence(Hex, Hex2, Hex3, Hex4);
  }

  public Uchar_Sequence withHex2(hydra.ext.io.shex.syntax.Hex Hex2) {
    return new Uchar_Sequence(Hex, Hex2, Hex3, Hex4);
  }

  public Uchar_Sequence withHex3(hydra.ext.io.shex.syntax.Hex Hex3) {
    return new Uchar_Sequence(Hex, Hex2, Hex3, Hex4);
  }

  public Uchar_Sequence withHex4(hydra.ext.io.shex.syntax.Hex Hex4) {
    return new Uchar_Sequence(Hex, Hex2, Hex3, Hex4);
  }
}
