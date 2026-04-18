// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DependentValueExpression implements Serializable, Comparable<DependentValueExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DependentValueExpression");

  public static final hydra.core.Name SET_QUANTIFIER = new hydra.core.Name("setQuantifier");

  public static final hydra.core.Name NUMERIC_VALUE = new hydra.core.Name("numericValue");

  public final hydra.util.Maybe<openGql.grammar.SetQuantifier> setQuantifier;

  public final openGql.grammar.NumericValueExpression numericValue;

  public DependentValueExpression (hydra.util.Maybe<openGql.grammar.SetQuantifier> setQuantifier, openGql.grammar.NumericValueExpression numericValue) {
    this.setQuantifier = setQuantifier;
    this.numericValue = numericValue;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DependentValueExpression)) {
      return false;
    }
    DependentValueExpression o = (DependentValueExpression) other;
    return java.util.Objects.equals(
      this.setQuantifier,
      o.setQuantifier) && java.util.Objects.equals(
      this.numericValue,
      o.numericValue);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(setQuantifier) + 3 * java.util.Objects.hashCode(numericValue);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DependentValueExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      setQuantifier,
      other.setQuantifier);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      numericValue,
      other.numericValue);
  }

  public DependentValueExpression withSetQuantifier(hydra.util.Maybe<openGql.grammar.SetQuantifier> setQuantifier) {
    return new DependentValueExpression(setQuantifier, numericValue);
  }

  public DependentValueExpression withNumericValue(openGql.grammar.NumericValueExpression numericValue) {
    return new DependentValueExpression(setQuantifier, numericValue);
  }
}
