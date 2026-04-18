// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class StringListNullPredicateExpression implements Serializable, Comparable<StringListNullPredicateExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.StringListNullPredicateExpression");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final hydra.cypher.openCypher.AddOrSubtractExpression left;

  public final java.util.List<hydra.cypher.openCypher.StringListNullPredicateRightHandSide> right;

  public StringListNullPredicateExpression (hydra.cypher.openCypher.AddOrSubtractExpression left, java.util.List<hydra.cypher.openCypher.StringListNullPredicateRightHandSide> right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringListNullPredicateExpression)) {
      return false;
    }
    StringListNullPredicateExpression o = (StringListNullPredicateExpression) other;
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
  public int compareTo(StringListNullPredicateExpression other) {
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

  public StringListNullPredicateExpression withLeft(hydra.cypher.openCypher.AddOrSubtractExpression left) {
    return new StringListNullPredicateExpression(left, right);
  }

  public StringListNullPredicateExpression withRight(java.util.List<hydra.cypher.openCypher.StringListNullPredicateRightHandSide> right) {
    return new StringListNullPredicateExpression(left, right);
  }
}
