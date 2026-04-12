// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NumericFacet_Sequence2 implements Serializable, Comparable<NumericFacet_Sequence2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NumericFacet_Sequence2");

  public static final hydra.core.Name NUMERIC_LENGTH = new hydra.core.Name("NumericLength");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("Integer");

  public final hydra.ext.io.shex.syntax.NumericLength NumericLength;

  public final hydra.ext.io.shex.syntax.Integer_ Integer_;

  public NumericFacet_Sequence2 (hydra.ext.io.shex.syntax.NumericLength NumericLength, hydra.ext.io.shex.syntax.Integer_ Integer_) {
    this.NumericLength = NumericLength;
    this.Integer_ = Integer_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NumericFacet_Sequence2)) {
      return false;
    }
    NumericFacet_Sequence2 o = (NumericFacet_Sequence2) other;
    return java.util.Objects.equals(
      this.NumericLength,
      o.NumericLength) && java.util.Objects.equals(
      this.Integer_,
      o.Integer_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(NumericLength) + 3 * java.util.Objects.hashCode(Integer_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NumericFacet_Sequence2 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      NumericLength,
      other.NumericLength);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      Integer_,
      other.Integer_);
  }

  public NumericFacet_Sequence2 withNumericLength(hydra.ext.io.shex.syntax.NumericLength NumericLength) {
    return new NumericFacet_Sequence2(NumericLength, Integer_);
  }

  public NumericFacet_Sequence2 withInteger(hydra.ext.io.shex.syntax.Integer_ Integer_) {
    return new NumericFacet_Sequence2(NumericLength, Integer_);
  }
}
