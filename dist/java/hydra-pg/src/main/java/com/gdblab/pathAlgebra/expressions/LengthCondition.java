// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Condition on path length: len() = i
 */
public class LengthCondition implements Serializable, Comparable<LengthCondition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.LengthCondition");

  public static final hydra.core.Name LENGTH = new hydra.core.Name("length");

  public final Integer length;

  public LengthCondition (Integer length) {
    this.length = length;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LengthCondition)) {
      return false;
    }
    LengthCondition o = (LengthCondition) other;
    return java.util.Objects.equals(
      this.length,
      o.length);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(length);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LengthCondition other) {
    return hydra.util.Comparing.compare(
      length,
      other.length);
  }
}
