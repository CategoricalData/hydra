// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class GroupBy implements Serializable, Comparable<GroupBy> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.GroupBy");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final com.gdblab.pathAlgebra.syntax.GroupByOption value;

  public GroupBy (com.gdblab.pathAlgebra.syntax.GroupByOption value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GroupBy)) {
      return false;
    }
    GroupBy o = (GroupBy) other;
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
  public int compareTo(GroupBy other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
