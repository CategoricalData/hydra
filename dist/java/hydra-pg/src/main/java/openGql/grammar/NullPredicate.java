// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NullPredicate implements Serializable, Comparable<NullPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NullPredicate");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name NULL_PART = new hydra.core.Name("nullPart");

  public final openGql.grammar.PrimaryValueExpression valueExpression;

  public final openGql.grammar.NullPredicatePart2 nullPart;

  public NullPredicate (openGql.grammar.PrimaryValueExpression valueExpression, openGql.grammar.NullPredicatePart2 nullPart) {
    this.valueExpression = valueExpression;
    this.nullPart = nullPart;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NullPredicate)) {
      return false;
    }
    NullPredicate o = (NullPredicate) other;
    return java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression) && java.util.Objects.equals(
      this.nullPart,
      o.nullPart);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(valueExpression) + 3 * java.util.Objects.hashCode(nullPart);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NullPredicate other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      nullPart,
      other.nullPart);
  }

  public NullPredicate withValueExpression(openGql.grammar.PrimaryValueExpression valueExpression) {
    return new NullPredicate(valueExpression, nullPart);
  }

  public NullPredicate withNullPart(openGql.grammar.NullPredicatePart2 nullPart) {
    return new NullPredicate(valueExpression, nullPart);
  }
}
