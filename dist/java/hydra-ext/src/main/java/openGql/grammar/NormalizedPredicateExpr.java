// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NormalizedPredicateExpr implements Serializable, Comparable<NormalizedPredicateExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NormalizedPredicateExpr");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name NORMALIZED_PREDICATE = new hydra.core.Name("normalizedPredicate");

  public final openGql.grammar.ValueExpression valueExpression;

  public final openGql.grammar.NormalizedPredicatePart2 normalizedPredicate;

  public NormalizedPredicateExpr (openGql.grammar.ValueExpression valueExpression, openGql.grammar.NormalizedPredicatePart2 normalizedPredicate) {
    this.valueExpression = valueExpression;
    this.normalizedPredicate = normalizedPredicate;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalizedPredicateExpr)) {
      return false;
    }
    NormalizedPredicateExpr o = (NormalizedPredicateExpr) other;
    return java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression) && java.util.Objects.equals(
      this.normalizedPredicate,
      o.normalizedPredicate);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(valueExpression) + 3 * java.util.Objects.hashCode(normalizedPredicate);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NormalizedPredicateExpr other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      normalizedPredicate,
      other.normalizedPredicate);
  }

  public NormalizedPredicateExpr withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new NormalizedPredicateExpr(valueExpression, normalizedPredicate);
  }

  public NormalizedPredicateExpr withNormalizedPredicate(openGql.grammar.NormalizedPredicatePart2 normalizedPredicate) {
    return new NormalizedPredicateExpr(valueExpression, normalizedPredicate);
  }
}
