// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TrimListFunction implements Serializable, Comparable<TrimListFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TrimListFunction");

  public static final hydra.core.Name LIST_VALUE = new hydra.core.Name("listValue");

  public static final hydra.core.Name NUMERIC_VALUE = new hydra.core.Name("numericValue");

  public final openGql.grammar.ValueExpression listValue;

  public final openGql.grammar.NumericValueExpression numericValue;

  public TrimListFunction (openGql.grammar.ValueExpression listValue, openGql.grammar.NumericValueExpression numericValue) {
    this.listValue = listValue;
    this.numericValue = numericValue;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TrimListFunction)) {
      return false;
    }
    TrimListFunction o = (TrimListFunction) other;
    return java.util.Objects.equals(
      this.listValue,
      o.listValue) && java.util.Objects.equals(
      this.numericValue,
      o.numericValue);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(listValue) + 3 * java.util.Objects.hashCode(numericValue);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TrimListFunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      listValue,
      other.listValue);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      numericValue,
      other.numericValue);
  }

  public TrimListFunction withListValue(openGql.grammar.ValueExpression listValue) {
    return new TrimListFunction(listValue, numericValue);
  }

  public TrimListFunction withNumericValue(openGql.grammar.NumericValueExpression numericValue) {
    return new TrimListFunction(listValue, numericValue);
  }
}
