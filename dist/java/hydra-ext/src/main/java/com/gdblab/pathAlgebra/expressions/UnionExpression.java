// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Union operator: expr1 ∪ expr2
 */
public class UnionExpression implements Serializable, Comparable<UnionExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.UnionExpression");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final com.gdblab.pathAlgebra.expressions.PathExpression left;

  public final com.gdblab.pathAlgebra.expressions.PathExpression right;

  public UnionExpression (com.gdblab.pathAlgebra.expressions.PathExpression left, com.gdblab.pathAlgebra.expressions.PathExpression right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionExpression)) {
      return false;
    }
    UnionExpression o = (UnionExpression) other;
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
  public int compareTo(UnionExpression other) {
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

  public UnionExpression withLeft(com.gdblab.pathAlgebra.expressions.PathExpression left) {
    return new UnionExpression(left, right);
  }

  public UnionExpression withRight(com.gdblab.pathAlgebra.expressions.PathExpression right) {
    return new UnionExpression(left, right);
  }
}
