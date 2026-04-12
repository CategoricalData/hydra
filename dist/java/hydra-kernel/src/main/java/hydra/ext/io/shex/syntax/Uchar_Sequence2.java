// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class Uchar_Sequence2 implements Serializable, Comparable<Uchar_Sequence2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar_Sequence2");

  public static final hydra.core.Name HEX = new hydra.core.Name("Hex");

  public static final hydra.core.Name HEX2 = new hydra.core.Name("Hex2");

  public static final hydra.core.Name HEX3 = new hydra.core.Name("Hex3");

  public static final hydra.core.Name HEX4 = new hydra.core.Name("Hex4");

  public static final hydra.core.Name HEX5 = new hydra.core.Name("Hex5");

  public static final hydra.core.Name HEX6 = new hydra.core.Name("Hex6");

  public static final hydra.core.Name HEX7 = new hydra.core.Name("Hex7");

  public static final hydra.core.Name HEX8 = new hydra.core.Name("Hex8");

  public final hydra.ext.io.shex.syntax.Hex Hex;

  public final hydra.ext.io.shex.syntax.Hex Hex2;

  public final hydra.ext.io.shex.syntax.Hex Hex3;

  public final hydra.ext.io.shex.syntax.Hex Hex4;

  public final hydra.ext.io.shex.syntax.Hex Hex5;

  public final hydra.ext.io.shex.syntax.Hex Hex6;

  public final hydra.ext.io.shex.syntax.Hex Hex7;

  public final hydra.ext.io.shex.syntax.Hex Hex8;

  public Uchar_Sequence2 (hydra.ext.io.shex.syntax.Hex Hex, hydra.ext.io.shex.syntax.Hex Hex2, hydra.ext.io.shex.syntax.Hex Hex3, hydra.ext.io.shex.syntax.Hex Hex4, hydra.ext.io.shex.syntax.Hex Hex5, hydra.ext.io.shex.syntax.Hex Hex6, hydra.ext.io.shex.syntax.Hex Hex7, hydra.ext.io.shex.syntax.Hex Hex8) {
    this.Hex = Hex;
    this.Hex2 = Hex2;
    this.Hex3 = Hex3;
    this.Hex4 = Hex4;
    this.Hex5 = Hex5;
    this.Hex6 = Hex6;
    this.Hex7 = Hex7;
    this.Hex8 = Hex8;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Uchar_Sequence2)) {
      return false;
    }
    Uchar_Sequence2 o = (Uchar_Sequence2) other;
    return java.util.Objects.equals(
      this.Hex,
      o.Hex) && java.util.Objects.equals(
      this.Hex2,
      o.Hex2) && java.util.Objects.equals(
      this.Hex3,
      o.Hex3) && java.util.Objects.equals(
      this.Hex4,
      o.Hex4) && java.util.Objects.equals(
      this.Hex5,
      o.Hex5) && java.util.Objects.equals(
      this.Hex6,
      o.Hex6) && java.util.Objects.equals(
      this.Hex7,
      o.Hex7) && java.util.Objects.equals(
      this.Hex8,
      o.Hex8);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Hex) + 3 * java.util.Objects.hashCode(Hex2) + 5 * java.util.Objects.hashCode(Hex3) + 7 * java.util.Objects.hashCode(Hex4) + 11 * java.util.Objects.hashCode(Hex5) + 13 * java.util.Objects.hashCode(Hex6) + 17 * java.util.Objects.hashCode(Hex7) + 19 * java.util.Objects.hashCode(Hex8);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Uchar_Sequence2 other) {
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
    cmp = hydra.util.Comparing.compare(
      Hex4,
      other.Hex4);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      Hex5,
      other.Hex5);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      Hex6,
      other.Hex6);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      Hex7,
      other.Hex7);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      Hex8,
      other.Hex8);
  }

  public Uchar_Sequence2 withHex(hydra.ext.io.shex.syntax.Hex Hex) {
    return new Uchar_Sequence2(Hex, Hex2, Hex3, Hex4, Hex5, Hex6, Hex7, Hex8);
  }

  public Uchar_Sequence2 withHex2(hydra.ext.io.shex.syntax.Hex Hex2) {
    return new Uchar_Sequence2(Hex, Hex2, Hex3, Hex4, Hex5, Hex6, Hex7, Hex8);
  }

  public Uchar_Sequence2 withHex3(hydra.ext.io.shex.syntax.Hex Hex3) {
    return new Uchar_Sequence2(Hex, Hex2, Hex3, Hex4, Hex5, Hex6, Hex7, Hex8);
  }

  public Uchar_Sequence2 withHex4(hydra.ext.io.shex.syntax.Hex Hex4) {
    return new Uchar_Sequence2(Hex, Hex2, Hex3, Hex4, Hex5, Hex6, Hex7, Hex8);
  }

  public Uchar_Sequence2 withHex5(hydra.ext.io.shex.syntax.Hex Hex5) {
    return new Uchar_Sequence2(Hex, Hex2, Hex3, Hex4, Hex5, Hex6, Hex7, Hex8);
  }

  public Uchar_Sequence2 withHex6(hydra.ext.io.shex.syntax.Hex Hex6) {
    return new Uchar_Sequence2(Hex, Hex2, Hex3, Hex4, Hex5, Hex6, Hex7, Hex8);
  }

  public Uchar_Sequence2 withHex7(hydra.ext.io.shex.syntax.Hex Hex7) {
    return new Uchar_Sequence2(Hex, Hex2, Hex3, Hex4, Hex5, Hex6, Hex7, Hex8);
  }

  public Uchar_Sequence2 withHex8(hydra.ext.io.shex.syntax.Hex Hex8) {
    return new Uchar_Sequence2(Hex, Hex2, Hex3, Hex4, Hex5, Hex6, Hex7, Hex8);
  }
}
