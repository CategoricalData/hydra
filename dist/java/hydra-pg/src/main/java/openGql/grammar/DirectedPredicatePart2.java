// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DirectedPredicatePart2 implements Serializable, Comparable<DirectedPredicatePart2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DirectedPredicatePart2");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public final Boolean not;

  public DirectedPredicatePart2 (Boolean not) {
    this.not = not;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DirectedPredicatePart2)) {
      return false;
    }
    DirectedPredicatePart2 o = (DirectedPredicatePart2) other;
    return java.util.Objects.equals(
      this.not,
      o.not);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(not);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DirectedPredicatePart2 other) {
    return hydra.util.Comparing.compare(
      not,
      other.not);
  }
}
