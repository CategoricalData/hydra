// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A binary operation expression
 */
public class BinaryExpression implements Serializable, Comparable<BinaryExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.BinaryExpression");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final hydra.javaScript.syntax.BinaryOperator operator;

  public final hydra.javaScript.syntax.Expression left;

  public final hydra.javaScript.syntax.Expression right;

  public BinaryExpression (hydra.javaScript.syntax.BinaryOperator operator, hydra.javaScript.syntax.Expression left, hydra.javaScript.syntax.Expression right) {
    this.operator = operator;
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryExpression)) {
      return false;
    }
    BinaryExpression o = (BinaryExpression) other;
    return java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operator) + 3 * java.util.Objects.hashCode(left) + 5 * java.util.Objects.hashCode(right);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BinaryExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      operator,
      other.operator);
    if (cmp != 0) {
      return cmp;
    }
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

  public BinaryExpression withOperator(hydra.javaScript.syntax.BinaryOperator operator) {
    return new BinaryExpression(operator, left, right);
  }

  public BinaryExpression withLeft(hydra.javaScript.syntax.Expression left) {
    return new BinaryExpression(operator, left, right);
  }

  public BinaryExpression withRight(hydra.javaScript.syntax.Expression right) {
    return new BinaryExpression(operator, left, right);
  }
}
