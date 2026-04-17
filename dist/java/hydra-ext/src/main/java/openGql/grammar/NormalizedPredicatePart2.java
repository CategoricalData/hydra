// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NormalizedPredicatePart2 implements Serializable, Comparable<NormalizedPredicatePart2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NormalizedPredicatePart2");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public static final hydra.core.Name NORMAL_FORM = new hydra.core.Name("normalForm");

  public final Boolean not;

  public final hydra.util.Maybe<openGql.grammar.NormalForm> normalForm;

  public NormalizedPredicatePart2 (Boolean not, hydra.util.Maybe<openGql.grammar.NormalForm> normalForm) {
    this.not = not;
    this.normalForm = normalForm;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalizedPredicatePart2)) {
      return false;
    }
    NormalizedPredicatePart2 o = (NormalizedPredicatePart2) other;
    return java.util.Objects.equals(
      this.not,
      o.not) && java.util.Objects.equals(
      this.normalForm,
      o.normalForm);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(not) + 3 * java.util.Objects.hashCode(normalForm);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NormalizedPredicatePart2 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      not,
      other.not);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      normalForm,
      other.normalForm);
  }

  public NormalizedPredicatePart2 withNot(Boolean not) {
    return new NormalizedPredicatePart2(not, normalForm);
  }

  public NormalizedPredicatePart2 withNormalForm(hydra.util.Maybe<openGql.grammar.NormalForm> normalForm) {
    return new NormalizedPredicatePart2(not, normalForm);
  }
}
