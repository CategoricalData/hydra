// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class StringFacet_Sequence implements Serializable, Comparable<StringFacet_Sequence> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.StringFacet_Sequence");

  public static final hydra.core.Name STRING_LENGTH = new hydra.core.Name("StringLength");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("Integer");

  public final hydra.ext.io.shex.syntax.StringLength StringLength;

  public final hydra.ext.io.shex.syntax.Integer_ Integer_;

  public StringFacet_Sequence (hydra.ext.io.shex.syntax.StringLength StringLength, hydra.ext.io.shex.syntax.Integer_ Integer_) {
    this.StringLength = StringLength;
    this.Integer_ = Integer_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringFacet_Sequence)) {
      return false;
    }
    StringFacet_Sequence o = (StringFacet_Sequence) other;
    return java.util.Objects.equals(
      this.StringLength,
      o.StringLength) && java.util.Objects.equals(
      this.Integer_,
      o.Integer_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(StringLength) + 3 * java.util.Objects.hashCode(Integer_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringFacet_Sequence other) {
    int cmp = 0;
    cmp = ((Comparable) StringLength).compareTo(other.StringLength);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) Integer_).compareTo(other.Integer_);
  }

  public StringFacet_Sequence withStringLength(hydra.ext.io.shex.syntax.StringLength StringLength) {
    return new StringFacet_Sequence(StringLength, Integer_);
  }

  public StringFacet_Sequence withInteger(hydra.ext.io.shex.syntax.Integer_ Integer_) {
    return new StringFacet_Sequence(StringLength, Integer_);
  }
}
