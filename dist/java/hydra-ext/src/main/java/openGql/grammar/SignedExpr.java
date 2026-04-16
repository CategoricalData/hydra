// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SignedExpr implements Serializable, Comparable<SignedExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SignedExpr");

  public static final hydra.core.Name SIGN = new hydra.core.Name("sign");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public final openGql.grammar.Sign sign;

  public final openGql.grammar.ValueExpression valueExpression;

  public SignedExpr (openGql.grammar.Sign sign, openGql.grammar.ValueExpression valueExpression) {
    this.sign = sign;
    this.valueExpression = valueExpression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SignedExpr)) {
      return false;
    }
    SignedExpr o = (SignedExpr) other;
    return java.util.Objects.equals(
      this.sign,
      o.sign) && java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(sign) + 3 * java.util.Objects.hashCode(valueExpression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SignedExpr other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      sign,
      other.sign);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
  }

  public SignedExpr withSign(openGql.grammar.Sign sign) {
    return new SignedExpr(sign, valueExpression);
  }

  public SignedExpr withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new SignedExpr(sign, valueExpression);
  }
}
