// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Order-by operator: τ_criterion(solutionSpace)
 */
public class OrderByExpression implements Serializable, Comparable<OrderByExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.OrderByExpression");

  public static final hydra.core.Name CRITERION = new hydra.core.Name("criterion");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final com.gdblab.pathAlgebra.expressions.OrderByCriterion criterion;

  public final com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression expression;

  public OrderByExpression (com.gdblab.pathAlgebra.expressions.OrderByCriterion criterion, com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression expression) {
    this.criterion = criterion;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrderByExpression)) {
      return false;
    }
    OrderByExpression o = (OrderByExpression) other;
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
  public int compareTo(OrderByExpression other) {
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

  public OrderByExpression withCriterion(com.gdblab.pathAlgebra.expressions.OrderByCriterion criterion) {
    return new OrderByExpression(criterion, expression);
  }

  public OrderByExpression withExpression(com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression expression) {
    return new OrderByExpression(criterion, expression);
  }
}
