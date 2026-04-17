// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SourcePredicate implements Serializable, Comparable<SourcePredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SourcePredicate");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public static final hydra.core.Name SOURCE_OF = new hydra.core.Name("sourceOf");

  public final Boolean not;

  public final String sourceOf;

  public SourcePredicate (Boolean not, String sourceOf) {
    this.not = not;
    this.sourceOf = sourceOf;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SourcePredicate)) {
      return false;
    }
    SourcePredicate o = (SourcePredicate) other;
    return java.util.Objects.equals(
      this.not,
      o.not) && java.util.Objects.equals(
      this.sourceOf,
      o.sourceOf);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(not) + 3 * java.util.Objects.hashCode(sourceOf);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SourcePredicate other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      not,
      other.not);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      sourceOf,
      other.sourceOf);
  }

  public SourcePredicate withNot(Boolean not) {
    return new SourcePredicate(not, sourceOf);
  }

  public SourcePredicate withSourceOf(String sourceOf) {
    return new SourcePredicate(not, sourceOf);
  }
}
