// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CompositeQueryExpressionConjunction implements Serializable, Comparable<CompositeQueryExpressionConjunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CompositeQueryExpressionConjunction");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name CONJUNCTION = new hydra.core.Name("conjunction");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final openGql.grammar.CompositeQueryExpression left;

  public final openGql.grammar.QueryConjunction conjunction;

  public final openGql.grammar.LinearQueryStatement right;

  public CompositeQueryExpressionConjunction (openGql.grammar.CompositeQueryExpression left, openGql.grammar.QueryConjunction conjunction, openGql.grammar.LinearQueryStatement right) {
    this.left = left;
    this.conjunction = conjunction;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CompositeQueryExpressionConjunction)) {
      return false;
    }
    CompositeQueryExpressionConjunction o = (CompositeQueryExpressionConjunction) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.conjunction,
      o.conjunction) && java.util.Objects.equals(
      this.right,
      o.right);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(conjunction) + 5 * java.util.Objects.hashCode(right);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CompositeQueryExpressionConjunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      left,
      other.left);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      conjunction,
      other.conjunction);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      right,
      other.right);
  }

  public CompositeQueryExpressionConjunction withLeft(openGql.grammar.CompositeQueryExpression left) {
    return new CompositeQueryExpressionConjunction(left, conjunction, right);
  }

  public CompositeQueryExpressionConjunction withConjunction(openGql.grammar.QueryConjunction conjunction) {
    return new CompositeQueryExpressionConjunction(left, conjunction, right);
  }

  public CompositeQueryExpressionConjunction withRight(openGql.grammar.LinearQueryStatement right) {
    return new CompositeQueryExpressionConjunction(left, conjunction, right);
  }
}
