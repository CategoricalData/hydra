// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ValueTypePredicate implements Serializable, Comparable<ValueTypePredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ValueTypePredicate");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name VALUE_TYPE_PART = new hydra.core.Name("valueTypePart");

  public final openGql.grammar.PrimaryValueExpression valueExpression;

  public final openGql.grammar.ValueTypePredicatePart2 valueTypePart;

  public ValueTypePredicate (openGql.grammar.PrimaryValueExpression valueExpression, openGql.grammar.ValueTypePredicatePart2 valueTypePart) {
    this.valueExpression = valueExpression;
    this.valueTypePart = valueTypePart;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValueTypePredicate)) {
      return false;
    }
    ValueTypePredicate o = (ValueTypePredicate) other;
    return java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression) && java.util.Objects.equals(
      this.valueTypePart,
      o.valueTypePart);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(valueExpression) + 3 * java.util.Objects.hashCode(valueTypePart);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ValueTypePredicate other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      valueTypePart,
      other.valueTypePart);
  }

  public ValueTypePredicate withValueExpression(openGql.grammar.PrimaryValueExpression valueExpression) {
    return new ValueTypePredicate(valueExpression, valueTypePart);
  }

  public ValueTypePredicate withValueTypePart(openGql.grammar.ValueTypePredicatePart2 valueTypePart) {
    return new ValueTypePredicate(valueExpression, valueTypePart);
  }
}
