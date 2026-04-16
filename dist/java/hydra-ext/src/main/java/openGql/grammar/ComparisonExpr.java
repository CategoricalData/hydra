// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ComparisonExpr implements Serializable, Comparable<ComparisonExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ComparisonExpr");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name COMPARISON = new hydra.core.Name("comparison");

  public final openGql.grammar.ValueExpression valueExpression;

  public final openGql.grammar.ComparisonPredicatePart2 comparison;

  public ComparisonExpr (openGql.grammar.ValueExpression valueExpression, openGql.grammar.ComparisonPredicatePart2 comparison) {
    this.valueExpression = valueExpression;
    this.comparison = comparison;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ComparisonExpr)) {
      return false;
    }
    ComparisonExpr o = (ComparisonExpr) other;
    return java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression) && java.util.Objects.equals(
      this.comparison,
      o.comparison);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(valueExpression) + 3 * java.util.Objects.hashCode(comparison);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ComparisonExpr other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      comparison,
      other.comparison);
  }

  public ComparisonExpr withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new ComparisonExpr(valueExpression, comparison);
  }

  public ComparisonExpr withComparison(openGql.grammar.ComparisonPredicatePart2 comparison) {
    return new ComparisonExpr(valueExpression, comparison);
  }
}
