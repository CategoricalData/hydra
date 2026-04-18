// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

public class AndCondition implements Serializable, Comparable<AndCondition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.AndCondition");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final com.gdblab.pathAlgebra.expressions.SelectionCondition left;

  public final com.gdblab.pathAlgebra.expressions.SelectionCondition right;

  public AndCondition (com.gdblab.pathAlgebra.expressions.SelectionCondition left, com.gdblab.pathAlgebra.expressions.SelectionCondition right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AndCondition)) {
      return false;
    }
    AndCondition o = (AndCondition) other;
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
  public int compareTo(AndCondition other) {
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

  public AndCondition withLeft(com.gdblab.pathAlgebra.expressions.SelectionCondition left) {
    return new AndCondition(left, right);
  }

  public AndCondition withRight(com.gdblab.pathAlgebra.expressions.SelectionCondition right) {
    return new AndCondition(left, right);
  }
}
