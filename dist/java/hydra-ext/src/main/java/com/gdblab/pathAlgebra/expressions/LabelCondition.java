// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Conditions on node/edge labels: label(node(i)) = v
 */
public class LabelCondition implements Serializable, Comparable<LabelCondition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.LabelCondition");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final com.gdblab.pathAlgebra.expressions.PathElement target;

  public final String value;

  public LabelCondition (com.gdblab.pathAlgebra.expressions.PathElement target, String value) {
    this.target = target;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabelCondition)) {
      return false;
    }
    LabelCondition o = (LabelCondition) other;
    return java.util.Objects.equals(
      this.target,
      o.target) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(target) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LabelCondition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      target,
      other.target);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public LabelCondition withTarget(com.gdblab.pathAlgebra.expressions.PathElement target) {
    return new LabelCondition(target, value);
  }

  public LabelCondition withValue(String value) {
    return new LabelCondition(target, value);
  }
}
