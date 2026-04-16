// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ConcatenationExpr implements Serializable, Comparable<ConcatenationExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ConcatenationExpr");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final openGql.grammar.ValueExpression left;

  public final openGql.grammar.ValueExpression right;

  public ConcatenationExpr (openGql.grammar.ValueExpression left, openGql.grammar.ValueExpression right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConcatenationExpr)) {
      return false;
    }
    ConcatenationExpr o = (ConcatenationExpr) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConcatenationExpr other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      left,
      other.left);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      right,
      other.right);
  }

  public ConcatenationExpr withLeft(openGql.grammar.ValueExpression left) {
    return new ConcatenationExpr(left, right);
  }

  public ConcatenationExpr withRight(openGql.grammar.ValueExpression right) {
    return new ConcatenationExpr(left, right);
  }
}
