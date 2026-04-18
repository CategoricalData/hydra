// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

public class NotCondition implements Serializable, Comparable<NotCondition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.NotCondition");

  public static final hydra.core.Name CONDITION = new hydra.core.Name("condition");

  public final com.gdblab.pathAlgebra.expressions.SelectionCondition condition;

  public NotCondition (com.gdblab.pathAlgebra.expressions.SelectionCondition condition) {
    this.condition = condition;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NotCondition)) {
      return false;
    }
    NotCondition o = (NotCondition) other;
    return java.util.Objects.equals(
      this.condition,
      o.condition);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(condition);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NotCondition other) {
    return hydra.util.Comparing.compare(
      condition,
      other.condition);
  }
}
