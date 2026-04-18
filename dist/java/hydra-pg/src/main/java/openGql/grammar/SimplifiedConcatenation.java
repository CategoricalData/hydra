// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SimplifiedConcatenation implements Serializable, Comparable<SimplifiedConcatenation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedConcatenation");

  public static final hydra.core.Name INITIAL_TERM = new hydra.core.Name("initialTerm");

  public static final hydra.core.Name NEXT_FACTOR = new hydra.core.Name("nextFactor");

  public final openGql.grammar.SimplifiedTerm initialTerm;

  public final openGql.grammar.SimplifiedFactorLow nextFactor;

  public SimplifiedConcatenation (openGql.grammar.SimplifiedTerm initialTerm, openGql.grammar.SimplifiedFactorLow nextFactor) {
    this.initialTerm = initialTerm;
    this.nextFactor = nextFactor;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimplifiedConcatenation)) {
      return false;
    }
    SimplifiedConcatenation o = (SimplifiedConcatenation) other;
    return java.util.Objects.equals(
      this.initialTerm,
      o.initialTerm) && java.util.Objects.equals(
      this.nextFactor,
      o.nextFactor);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(initialTerm) + 3 * java.util.Objects.hashCode(nextFactor);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SimplifiedConcatenation other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      initialTerm,
      other.initialTerm);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      nextFactor,
      other.nextFactor);
  }

  public SimplifiedConcatenation withInitialTerm(openGql.grammar.SimplifiedTerm initialTerm) {
    return new SimplifiedConcatenation(initialTerm, nextFactor);
  }

  public SimplifiedConcatenation withNextFactor(openGql.grammar.SimplifiedFactorLow nextFactor) {
    return new SimplifiedConcatenation(initialTerm, nextFactor);
  }
}
