// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Recursive operator with path semantics
 */
public class RecursiveExpression implements Serializable, Comparable<RecursiveExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.RecursiveExpression");

  public static final hydra.core.Name SEMANTICS = new hydra.core.Name("semantics");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final com.gdblab.pathAlgebra.expressions.PathSemantics semantics;

  public final com.gdblab.pathAlgebra.expressions.PathExpression expression;

  public RecursiveExpression (com.gdblab.pathAlgebra.expressions.PathSemantics semantics, com.gdblab.pathAlgebra.expressions.PathExpression expression) {
    this.semantics = semantics;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecursiveExpression)) {
      return false;
    }
    RecursiveExpression o = (RecursiveExpression) other;
    return java.util.Objects.equals(
      this.semantics,
      o.semantics) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(semantics) + 3 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RecursiveExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      semantics,
      other.semantics);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public RecursiveExpression withSemantics(com.gdblab.pathAlgebra.expressions.PathSemantics semantics) {
    return new RecursiveExpression(semantics, expression);
  }

  public RecursiveExpression withExpression(com.gdblab.pathAlgebra.expressions.PathExpression expression) {
    return new RecursiveExpression(semantics, expression);
  }
}
