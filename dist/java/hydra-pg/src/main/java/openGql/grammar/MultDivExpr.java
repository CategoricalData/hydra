// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class MultDivExpr implements Serializable, Comparable<MultDivExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.MultDivExpr");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final openGql.grammar.ValueExpression left;

  public final openGql.grammar.MultDivOperator operator;

  public final openGql.grammar.ValueExpression right;

  public MultDivExpr (openGql.grammar.ValueExpression left, openGql.grammar.MultDivOperator operator, openGql.grammar.ValueExpression right) {
    this.left = left;
    this.operator = operator;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultDivExpr)) {
      return false;
    }
    MultDivExpr o = (MultDivExpr) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.right,
      o.right);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(operator) + 5 * java.util.Objects.hashCode(right);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MultDivExpr other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      left,
      other.left);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      operator,
      other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      right,
      other.right);
  }

  public MultDivExpr withLeft(openGql.grammar.ValueExpression left) {
    return new MultDivExpr(left, operator, right);
  }

  public MultDivExpr withOperator(openGql.grammar.MultDivOperator operator) {
    return new MultDivExpr(left, operator, right);
  }

  public MultDivExpr withRight(openGql.grammar.ValueExpression right) {
    return new MultDivExpr(left, operator, right);
  }
}
