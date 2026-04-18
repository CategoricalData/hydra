// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class QuantifiedPathPrimary implements Serializable, Comparable<QuantifiedPathPrimary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.QuantifiedPathPrimary");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  public static final hydra.core.Name QUANTIFIER = new hydra.core.Name("quantifier");

  public final openGql.grammar.PathPrimary primary;

  public final openGql.grammar.GraphPatternQuantifier quantifier;

  public QuantifiedPathPrimary (openGql.grammar.PathPrimary primary, openGql.grammar.GraphPatternQuantifier quantifier) {
    this.primary = primary;
    this.quantifier = quantifier;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QuantifiedPathPrimary)) {
      return false;
    }
    QuantifiedPathPrimary o = (QuantifiedPathPrimary) other;
    return java.util.Objects.equals(
      this.primary,
      o.primary) && java.util.Objects.equals(
      this.quantifier,
      o.quantifier);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(primary) + 3 * java.util.Objects.hashCode(quantifier);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(QuantifiedPathPrimary other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      primary,
      other.primary);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      quantifier,
      other.quantifier);
  }

  public QuantifiedPathPrimary withPrimary(openGql.grammar.PathPrimary primary) {
    return new QuantifiedPathPrimary(primary, quantifier);
  }

  public QuantifiedPathPrimary withQuantifier(openGql.grammar.GraphPatternQuantifier quantifier) {
    return new QuantifiedPathPrimary(primary, quantifier);
  }
}
