// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NullIfAbbreviation implements Serializable, Comparable<NullIfAbbreviation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NullIfAbbreviation");

  public static final hydra.core.Name FIRST = new hydra.core.Name("first");

  public static final hydra.core.Name SECOND = new hydra.core.Name("second");

  public final openGql.grammar.ValueExpression first;

  public final openGql.grammar.ValueExpression second;

  public NullIfAbbreviation (openGql.grammar.ValueExpression first, openGql.grammar.ValueExpression second) {
    this.first = first;
    this.second = second;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NullIfAbbreviation)) {
      return false;
    }
    NullIfAbbreviation o = (NullIfAbbreviation) other;
    return java.util.Objects.equals(
      this.first,
      o.first) && java.util.Objects.equals(
      this.second,
      o.second);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(first) + 3 * java.util.Objects.hashCode(second);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NullIfAbbreviation other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      first,
      other.first);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      second,
      other.second);
  }

  public NullIfAbbreviation withFirst(openGql.grammar.ValueExpression first) {
    return new NullIfAbbreviation(first, second);
  }

  public NullIfAbbreviation withSecond(openGql.grammar.ValueExpression second) {
    return new NullIfAbbreviation(first, second);
  }
}
