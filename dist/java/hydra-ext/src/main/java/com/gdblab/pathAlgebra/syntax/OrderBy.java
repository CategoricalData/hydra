// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class OrderBy implements Serializable, Comparable<OrderBy> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.OrderBy");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final com.gdblab.pathAlgebra.syntax.OrderByOption value;

  public OrderBy (com.gdblab.pathAlgebra.syntax.OrderByOption value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrderBy)) {
      return false;
    }
    OrderBy o = (OrderBy) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OrderBy other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
