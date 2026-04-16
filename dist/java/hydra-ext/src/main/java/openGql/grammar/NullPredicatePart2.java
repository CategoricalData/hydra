// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NullPredicatePart2 implements Serializable, Comparable<NullPredicatePart2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NullPredicatePart2");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public final Boolean not;

  public NullPredicatePart2 (Boolean not) {
    this.not = not;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NullPredicatePart2)) {
      return false;
    }
    NullPredicatePart2 o = (NullPredicatePart2) other;
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
  public int compareTo(NullPredicatePart2 other) {
    return hydra.util.Comparing.compare(
      not,
      other.not);
  }
}
