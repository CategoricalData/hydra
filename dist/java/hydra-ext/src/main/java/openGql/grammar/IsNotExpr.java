// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class IsNotExpr implements Serializable, Comparable<IsNotExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.IsNotExpr");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public static final hydra.core.Name TRUTH_VALUE = new hydra.core.Name("truthValue");

  public final openGql.grammar.ValueExpression valueExpression;

  public final Boolean not;

  public final openGql.grammar.BooleanLiteral truthValue;

  public IsNotExpr (openGql.grammar.ValueExpression valueExpression, Boolean not, openGql.grammar.BooleanLiteral truthValue) {
    this.valueExpression = valueExpression;
    this.not = not;
    this.truthValue = truthValue;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IsNotExpr)) {
      return false;
    }
    IsNotExpr o = (IsNotExpr) other;
    return java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression) && java.util.Objects.equals(
      this.not,
      o.not) && java.util.Objects.equals(
      this.truthValue,
      o.truthValue);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(valueExpression) + 3 * java.util.Objects.hashCode(not) + 5 * java.util.Objects.hashCode(truthValue);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IsNotExpr other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      not,
      other.not);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      truthValue,
      other.truthValue);
  }

  public IsNotExpr withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new IsNotExpr(valueExpression, not, truthValue);
  }

  public IsNotExpr withNot(Boolean not) {
    return new IsNotExpr(valueExpression, not, truthValue);
  }

  public IsNotExpr withTruthValue(openGql.grammar.BooleanLiteral truthValue) {
    return new IsNotExpr(valueExpression, not, truthValue);
  }
}
