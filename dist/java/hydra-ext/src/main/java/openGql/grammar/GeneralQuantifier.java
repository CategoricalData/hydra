// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class GeneralQuantifier implements Serializable, Comparable<GeneralQuantifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GeneralQuantifier");

  public static final hydra.core.Name LOWER_BOUND = new hydra.core.Name("lowerBound");

  public static final hydra.core.Name UPPER_BOUND = new hydra.core.Name("upperBound");

  public final hydra.util.Maybe<openGql.grammar.UnsignedInteger> lowerBound;

  public final hydra.util.Maybe<openGql.grammar.UnsignedInteger> upperBound;

  public GeneralQuantifier (hydra.util.Maybe<openGql.grammar.UnsignedInteger> lowerBound, hydra.util.Maybe<openGql.grammar.UnsignedInteger> upperBound) {
    this.lowerBound = lowerBound;
    this.upperBound = upperBound;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GeneralQuantifier)) {
      return false;
    }
    GeneralQuantifier o = (GeneralQuantifier) other;
    return java.util.Objects.equals(
      this.lowerBound,
      o.lowerBound) && java.util.Objects.equals(
      this.upperBound,
      o.upperBound);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lowerBound) + 3 * java.util.Objects.hashCode(upperBound);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GeneralQuantifier other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      lowerBound,
      other.lowerBound);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      upperBound,
      other.upperBound);
  }

  public GeneralQuantifier withLowerBound(hydra.util.Maybe<openGql.grammar.UnsignedInteger> lowerBound) {
    return new GeneralQuantifier(lowerBound, upperBound);
  }

  public GeneralQuantifier withUpperBound(hydra.util.Maybe<openGql.grammar.UnsignedInteger> upperBound) {
    return new GeneralQuantifier(lowerBound, upperBound);
  }
}
