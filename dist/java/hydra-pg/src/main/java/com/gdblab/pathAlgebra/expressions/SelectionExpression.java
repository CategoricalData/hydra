// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Selection operator: σ_condition(expression)
 */
public class SelectionExpression implements Serializable, Comparable<SelectionExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.SelectionExpression");

  public static final hydra.core.Name CONDITION = new hydra.core.Name("condition");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final com.gdblab.pathAlgebra.expressions.SelectionCondition condition;

  public final com.gdblab.pathAlgebra.expressions.PathExpression expression;

  public SelectionExpression (com.gdblab.pathAlgebra.expressions.SelectionCondition condition, com.gdblab.pathAlgebra.expressions.PathExpression expression) {
    this.condition = condition;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SelectionExpression)) {
      return false;
    }
    SelectionExpression o = (SelectionExpression) other;
    return java.util.Objects.equals(
      this.condition,
      o.condition) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(condition) + 3 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SelectionExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      condition,
      other.condition);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public SelectionExpression withCondition(com.gdblab.pathAlgebra.expressions.SelectionCondition condition) {
    return new SelectionExpression(condition, expression);
  }

  public SelectionExpression withExpression(com.gdblab.pathAlgebra.expressions.PathExpression expression) {
    return new SelectionExpression(condition, expression);
  }
}
