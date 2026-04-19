// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns.
 */
public class PatternImplication implements Serializable, Comparable<PatternImplication> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.query.PatternImplication");

  public static final hydra.core.Name ANTECEDENT = new hydra.core.Name("antecedent");

  public static final hydra.core.Name CONSEQUENT = new hydra.core.Name("consequent");

  /**
   * The pattern which, if it matches, triggers the constraint
   */
  public final hydra.query.Pattern antecedent;

  /**
   * The pattern which must also match when the antecedent matches
   */
  public final hydra.query.Pattern consequent;

  public PatternImplication (hydra.query.Pattern antecedent, hydra.query.Pattern consequent) {
    this.antecedent = antecedent;
    this.consequent = consequent;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternImplication)) {
      return false;
    }
    PatternImplication o = (PatternImplication) other;
    return java.util.Objects.equals(
      this.antecedent,
      o.antecedent) && java.util.Objects.equals(
      this.consequent,
      o.consequent);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(antecedent) + 3 * java.util.Objects.hashCode(consequent);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PatternImplication other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      antecedent,
      other.antecedent);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      consequent,
      other.consequent);
  }

  public PatternImplication withAntecedent(hydra.query.Pattern antecedent) {
    return new PatternImplication(antecedent, consequent);
  }

  public PatternImplication withConsequent(hydra.query.Pattern consequent) {
    return new PatternImplication(antecedent, consequent);
  }
}
