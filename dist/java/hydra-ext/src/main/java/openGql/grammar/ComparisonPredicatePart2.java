// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ComparisonPredicatePart2 implements Serializable, Comparable<ComparisonPredicatePart2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ComparisonPredicatePart2");

  public static final hydra.core.Name COMP_OP = new hydra.core.Name("compOp");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public final openGql.grammar.CompOp compOp;

  public final openGql.grammar.ValueExpression valueExpression;

  public ComparisonPredicatePart2 (openGql.grammar.CompOp compOp, openGql.grammar.ValueExpression valueExpression) {
    this.compOp = compOp;
    this.valueExpression = valueExpression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ComparisonPredicatePart2)) {
      return false;
    }
    ComparisonPredicatePart2 o = (ComparisonPredicatePart2) other;
    return java.util.Objects.equals(
      this.compOp,
      o.compOp) && java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(compOp) + 3 * java.util.Objects.hashCode(valueExpression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ComparisonPredicatePart2 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      compOp,
      other.compOp);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
  }

  public ComparisonPredicatePart2 withCompOp(openGql.grammar.CompOp compOp) {
    return new ComparisonPredicatePart2(compOp, valueExpression);
  }

  public ComparisonPredicatePart2 withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new ComparisonPredicatePart2(compOp, valueExpression);
  }
}
