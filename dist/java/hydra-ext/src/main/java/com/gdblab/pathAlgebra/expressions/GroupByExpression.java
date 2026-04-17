// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Group-by operator: γ_criterion(expression)
 */
public class GroupByExpression implements Serializable, Comparable<GroupByExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.GroupByExpression");

  public static final hydra.core.Name CRITERION = new hydra.core.Name("criterion");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final com.gdblab.pathAlgebra.expressions.GroupByCriterion criterion;

  public final com.gdblab.pathAlgebra.expressions.PathExpression expression;

  public GroupByExpression (com.gdblab.pathAlgebra.expressions.GroupByCriterion criterion, com.gdblab.pathAlgebra.expressions.PathExpression expression) {
    this.criterion = criterion;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GroupByExpression)) {
      return false;
    }
    GroupByExpression o = (GroupByExpression) other;
    return java.util.Objects.equals(
      this.criterion,
      o.criterion) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(criterion) + 3 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GroupByExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      criterion,
      other.criterion);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public GroupByExpression withCriterion(com.gdblab.pathAlgebra.expressions.GroupByCriterion criterion) {
    return new GroupByExpression(criterion, expression);
  }

  public GroupByExpression withExpression(com.gdblab.pathAlgebra.expressions.PathExpression expression) {
    return new GroupByExpression(criterion, expression);
  }
}
