// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DisjunctionLabelExpression implements Serializable, Comparable<DisjunctionLabelExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DisjunctionLabelExpression");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final openGql.grammar.LabelExpression left;

  public final openGql.grammar.LabelExpression right;

  public DisjunctionLabelExpression (openGql.grammar.LabelExpression left, openGql.grammar.LabelExpression right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DisjunctionLabelExpression)) {
      return false;
    }
    DisjunctionLabelExpression o = (DisjunctionLabelExpression) other;
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
  public int compareTo(DisjunctionLabelExpression other) {
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

  public DisjunctionLabelExpression withLeft(openGql.grammar.LabelExpression left) {
    return new DisjunctionLabelExpression(left, right);
  }

  public DisjunctionLabelExpression withRight(openGql.grammar.LabelExpression right) {
    return new DisjunctionLabelExpression(left, right);
  }
}
