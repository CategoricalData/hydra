// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DatetimeSubtractionParameters implements Serializable, Comparable<DatetimeSubtractionParameters> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DatetimeSubtractionParameters");

  public static final hydra.core.Name EXPRESSION1 = new hydra.core.Name("expression1");

  public static final hydra.core.Name EXPRESSION2 = new hydra.core.Name("expression2");

  public final openGql.grammar.ValueExpression expression1;

  public final openGql.grammar.ValueExpression expression2;

  public DatetimeSubtractionParameters (openGql.grammar.ValueExpression expression1, openGql.grammar.ValueExpression expression2) {
    this.expression1 = expression1;
    this.expression2 = expression2;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatetimeSubtractionParameters)) {
      return false;
    }
    DatetimeSubtractionParameters o = (DatetimeSubtractionParameters) other;
    return java.util.Objects.equals(
      this.expression1,
      o.expression1) && java.util.Objects.equals(
      this.expression2,
      o.expression2);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression1) + 3 * java.util.Objects.hashCode(expression2);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DatetimeSubtractionParameters other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expression1,
      other.expression1);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression2,
      other.expression2);
  }

  public DatetimeSubtractionParameters withExpression1(openGql.grammar.ValueExpression expression1) {
    return new DatetimeSubtractionParameters(expression1, expression2);
  }

  public DatetimeSubtractionParameters withExpression2(openGql.grammar.ValueExpression expression2) {
    return new DatetimeSubtractionParameters(expression1, expression2);
  }
}
