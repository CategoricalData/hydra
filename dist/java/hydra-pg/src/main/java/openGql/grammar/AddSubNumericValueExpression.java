// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class AddSubNumericValueExpression implements Serializable, Comparable<AddSubNumericValueExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AddSubNumericValueExpression");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final openGql.grammar.NumericValueExpression left;

  public final openGql.grammar.AddSubtractOperator operator;

  public final openGql.grammar.NumericValueExpression right;

  public AddSubNumericValueExpression (openGql.grammar.NumericValueExpression left, openGql.grammar.AddSubtractOperator operator, openGql.grammar.NumericValueExpression right) {
    this.left = left;
    this.operator = operator;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AddSubNumericValueExpression)) {
      return false;
    }
    AddSubNumericValueExpression o = (AddSubNumericValueExpression) other;
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
  public int compareTo(AddSubNumericValueExpression other) {
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

  public AddSubNumericValueExpression withLeft(openGql.grammar.NumericValueExpression left) {
    return new AddSubNumericValueExpression(left, operator, right);
  }

  public AddSubNumericValueExpression withOperator(openGql.grammar.AddSubtractOperator operator) {
    return new AddSubNumericValueExpression(left, operator, right);
  }

  public AddSubNumericValueExpression withRight(openGql.grammar.NumericValueExpression right) {
    return new AddSubNumericValueExpression(left, operator, right);
  }
}
