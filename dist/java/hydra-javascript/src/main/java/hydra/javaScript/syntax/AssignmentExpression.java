// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An assignment expression
 */
public class AssignmentExpression implements Serializable, Comparable<AssignmentExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.AssignmentExpression");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final hydra.javaScript.syntax.AssignmentOperator operator;

  public final hydra.javaScript.syntax.Pattern left;

  public final hydra.javaScript.syntax.Expression right;

  public AssignmentExpression (hydra.javaScript.syntax.AssignmentOperator operator, hydra.javaScript.syntax.Pattern left, hydra.javaScript.syntax.Expression right) {
    this.operator = operator;
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssignmentExpression)) {
      return false;
    }
    AssignmentExpression o = (AssignmentExpression) other;
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
  public int compareTo(AssignmentExpression other) {
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

  public AssignmentExpression withOperator(hydra.javaScript.syntax.AssignmentOperator operator) {
    return new AssignmentExpression(operator, left, right);
  }

  public AssignmentExpression withLeft(hydra.javaScript.syntax.Pattern left) {
    return new AssignmentExpression(operator, left, right);
  }

  public AssignmentExpression withRight(hydra.javaScript.syntax.Expression right) {
    return new AssignmentExpression(operator, left, right);
  }
}
