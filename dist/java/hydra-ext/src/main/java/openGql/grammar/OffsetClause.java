// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class OffsetClause implements Serializable, Comparable<OffsetClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OffsetClause");

  public static final hydra.core.Name SYNONYM = new hydra.core.Name("synonym");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final openGql.grammar.OffsetSynonym synonym;

  public final openGql.grammar.NonNegativeIntegerSpecification value;

  public OffsetClause (openGql.grammar.OffsetSynonym synonym, openGql.grammar.NonNegativeIntegerSpecification value) {
    this.synonym = synonym;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OffsetClause)) {
      return false;
    }
    OffsetClause o = (OffsetClause) other;
    return java.util.Objects.equals(
      this.synonym,
      o.synonym) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(synonym) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OffsetClause other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      synonym,
      other.synonym);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public OffsetClause withSynonym(openGql.grammar.OffsetSynonym synonym) {
    return new OffsetClause(synonym, value);
  }

  public OffsetClause withValue(openGql.grammar.NonNegativeIntegerSpecification value) {
    return new OffsetClause(synonym, value);
  }
}
