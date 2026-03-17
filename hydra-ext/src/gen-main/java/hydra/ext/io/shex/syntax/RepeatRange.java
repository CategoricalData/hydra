// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class RepeatRange implements Serializable, Comparable<RepeatRange> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.RepeatRange");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("Integer");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("Sequence");

  public final hydra.ext.io.shex.syntax.Integer_ Integer_;

  public final hydra.util.Maybe<hydra.util.Maybe<hydra.util.Maybe<hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option>>> Sequence;

  public RepeatRange (hydra.ext.io.shex.syntax.Integer_ Integer_, hydra.util.Maybe<hydra.util.Maybe<hydra.util.Maybe<hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option>>> Sequence) {
    this.Integer_ = Integer_;
    this.Sequence = Sequence;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RepeatRange)) {
      return false;
    }
    RepeatRange o = (RepeatRange) other;
    return java.util.Objects.equals(
      this.Integer_,
      o.Integer_) && java.util.Objects.equals(
      this.Sequence,
      o.Sequence);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Integer_) + 3 * java.util.Objects.hashCode(Sequence);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RepeatRange other) {
    int cmp = 0;
    cmp = ((Comparable) Integer_).compareTo(other.Integer_);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) Sequence).compareTo(other.Sequence);
  }

  public RepeatRange withInteger(hydra.ext.io.shex.syntax.Integer_ Integer_) {
    return new RepeatRange(Integer_, Sequence);
  }

  public RepeatRange withSequence(hydra.util.Maybe<hydra.util.Maybe<hydra.util.Maybe<hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option>>> Sequence) {
    return new RepeatRange(Integer_, Sequence);
  }
}
