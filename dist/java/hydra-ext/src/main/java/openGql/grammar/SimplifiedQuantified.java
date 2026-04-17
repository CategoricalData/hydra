// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SimplifiedQuantified implements Serializable, Comparable<SimplifiedQuantified> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedQuantified");

  public static final hydra.core.Name TERTIARY = new hydra.core.Name("tertiary");

  public static final hydra.core.Name QUANTIFIER = new hydra.core.Name("quantifier");

  public final openGql.grammar.SimplifiedTertiary tertiary;

  public final openGql.grammar.GraphPatternQuantifier quantifier;

  public SimplifiedQuantified (openGql.grammar.SimplifiedTertiary tertiary, openGql.grammar.GraphPatternQuantifier quantifier) {
    this.tertiary = tertiary;
    this.quantifier = quantifier;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimplifiedQuantified)) {
      return false;
    }
    SimplifiedQuantified o = (SimplifiedQuantified) other;
    return java.util.Objects.equals(
      this.tertiary,
      o.tertiary) && java.util.Objects.equals(
      this.quantifier,
      o.quantifier);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tertiary) + 3 * java.util.Objects.hashCode(quantifier);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SimplifiedQuantified other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      tertiary,
      other.tertiary);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      quantifier,
      other.quantifier);
  }

  public SimplifiedQuantified withTertiary(openGql.grammar.SimplifiedTertiary tertiary) {
    return new SimplifiedQuantified(tertiary, quantifier);
  }

  public SimplifiedQuantified withQuantifier(openGql.grammar.GraphPatternQuantifier quantifier) {
    return new SimplifiedQuantified(tertiary, quantifier);
  }
}
