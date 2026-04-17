// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DisjunctiveExpr implements Serializable, Comparable<DisjunctiveExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DisjunctiveExpr");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final openGql.grammar.ValueExpression left;

  public final openGql.grammar.DisjunctiveOperator operator;

  public final openGql.grammar.ValueExpression right;

  public DisjunctiveExpr (openGql.grammar.ValueExpression left, openGql.grammar.DisjunctiveOperator operator, openGql.grammar.ValueExpression right) {
    this.left = left;
    this.operator = operator;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DisjunctiveExpr)) {
      return false;
    }
    DisjunctiveExpr o = (DisjunctiveExpr) other;
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
  public int compareTo(DisjunctiveExpr other) {
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

  public DisjunctiveExpr withLeft(openGql.grammar.ValueExpression left) {
    return new DisjunctiveExpr(left, operator, right);
  }

  public DisjunctiveExpr withOperator(openGql.grammar.DisjunctiveOperator operator) {
    return new DisjunctiveExpr(left, operator, right);
  }

  public DisjunctiveExpr withRight(openGql.grammar.ValueExpression right) {
    return new DisjunctiveExpr(left, operator, right);
  }
}
