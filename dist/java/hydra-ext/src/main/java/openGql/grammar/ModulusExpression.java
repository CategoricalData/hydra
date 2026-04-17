// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ModulusExpression implements Serializable, Comparable<ModulusExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ModulusExpression");

  public static final hydra.core.Name DIVIDEND = new hydra.core.Name("dividend");

  public static final hydra.core.Name DIVISOR = new hydra.core.Name("divisor");

  public final openGql.grammar.NumericValueExpression dividend;

  public final openGql.grammar.NumericValueExpression divisor;

  public ModulusExpression (openGql.grammar.NumericValueExpression dividend, openGql.grammar.NumericValueExpression divisor) {
    this.dividend = dividend;
    this.divisor = divisor;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModulusExpression)) {
      return false;
    }
    ModulusExpression o = (ModulusExpression) other;
    return java.util.Objects.equals(
      this.dividend,
      o.dividend) && java.util.Objects.equals(
      this.divisor,
      o.divisor);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(dividend) + 3 * java.util.Objects.hashCode(divisor);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModulusExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      dividend,
      other.dividend);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      divisor,
      other.divisor);
  }

  public ModulusExpression withDividend(openGql.grammar.NumericValueExpression dividend) {
    return new ModulusExpression(dividend, divisor);
  }

  public ModulusExpression withDivisor(openGql.grammar.NumericValueExpression divisor) {
    return new ModulusExpression(dividend, divisor);
  }
}
