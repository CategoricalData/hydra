// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SamePredicate implements Serializable, Comparable<SamePredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SamePredicate");

  public static final hydra.core.Name REFERENCES = new hydra.core.Name("references");

  public final java.util.List<String> references;

  public SamePredicate (java.util.List<String> references) {
    this.references = references;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SamePredicate)) {
      return false;
    }
    SamePredicate o = (SamePredicate) other;
    return java.util.Objects.equals(
      this.references,
      o.references);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(references);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SamePredicate other) {
    return hydra.util.Comparing.compare(
      references,
      other.references);
  }
}
