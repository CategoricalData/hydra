// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class CompoundComplexCondition implements Serializable, Comparable<CompoundComplexCondition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.CompoundComplexCondition");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final com.gdblab.pathAlgebra.syntax.Condition lhs;

  public final com.gdblab.pathAlgebra.syntax.BoolOp operator;

  public final com.gdblab.pathAlgebra.syntax.ComplexCondition rhs;

  public CompoundComplexCondition (com.gdblab.pathAlgebra.syntax.Condition lhs, com.gdblab.pathAlgebra.syntax.BoolOp operator, com.gdblab.pathAlgebra.syntax.ComplexCondition rhs) {
    this.lhs = lhs;
    this.operator = operator;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CompoundComplexCondition)) {
      return false;
    }
    CompoundComplexCondition o = (CompoundComplexCondition) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(operator) + 5 * java.util.Objects.hashCode(rhs);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CompoundComplexCondition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      lhs,
      other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      operator,
      other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rhs,
      other.rhs);
  }

  public CompoundComplexCondition withLhs(com.gdblab.pathAlgebra.syntax.Condition lhs) {
    return new CompoundComplexCondition(lhs, operator, rhs);
  }

  public CompoundComplexCondition withOperator(com.gdblab.pathAlgebra.syntax.BoolOp operator) {
    return new CompoundComplexCondition(lhs, operator, rhs);
  }

  public CompoundComplexCondition withRhs(com.gdblab.pathAlgebra.syntax.ComplexCondition rhs) {
    return new CompoundComplexCondition(lhs, operator, rhs);
  }
}
