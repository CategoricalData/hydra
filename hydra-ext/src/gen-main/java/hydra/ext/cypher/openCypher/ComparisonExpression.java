// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class ComparisonExpression implements Serializable, Comparable<ComparisonExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.ComparisonExpression");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final hydra.ext.cypher.openCypher.StringListNullPredicateExpression left;

  public final java.util.List<hydra.ext.cypher.openCypher.PartialComparisonExpression> right;

  public ComparisonExpression (hydra.ext.cypher.openCypher.StringListNullPredicateExpression left, java.util.List<hydra.ext.cypher.openCypher.PartialComparisonExpression> right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ComparisonExpression)) {
      return false;
    }
    ComparisonExpression o = (ComparisonExpression) other;
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
  public int compareTo(ComparisonExpression other) {
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

  public ComparisonExpression withLeft(hydra.ext.cypher.openCypher.StringListNullPredicateExpression left) {
    return new ComparisonExpression(left, right);
  }

  public ComparisonExpression withRight(java.util.List<hydra.ext.cypher.openCypher.PartialComparisonExpression> right) {
    return new ComparisonExpression(left, right);
  }
}
