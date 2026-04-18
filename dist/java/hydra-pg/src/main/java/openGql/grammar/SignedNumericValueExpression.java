// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SignedNumericValueExpression implements Serializable, Comparable<SignedNumericValueExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SignedNumericValueExpression");

  public static final hydra.core.Name SIGN = new hydra.core.Name("sign");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final openGql.grammar.Sign sign;

  public final openGql.grammar.NumericValueExpression expression;

  public SignedNumericValueExpression (openGql.grammar.Sign sign, openGql.grammar.NumericValueExpression expression) {
    this.sign = sign;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SignedNumericValueExpression)) {
      return false;
    }
    SignedNumericValueExpression o = (SignedNumericValueExpression) other;
    return java.util.Objects.equals(
      this.sign,
      o.sign) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(sign) + 3 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SignedNumericValueExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      sign,
      other.sign);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public SignedNumericValueExpression withSign(openGql.grammar.Sign sign) {
    return new SignedNumericValueExpression(sign, expression);
  }

  public SignedNumericValueExpression withExpression(openGql.grammar.NumericValueExpression expression) {
    return new SignedNumericValueExpression(sign, expression);
  }
}
