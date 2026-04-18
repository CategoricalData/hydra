// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class NestedFunction implements Serializable, Comparable<NestedFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.NestedFunction");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name INNER_FUNCTION = new hydra.core.Name("innerFunction");

  public final String name;

  public final com.gdblab.pathAlgebra.syntax.Function innerFunction;

  public NestedFunction (String name, com.gdblab.pathAlgebra.syntax.Function innerFunction) {
    this.name = name;
    this.innerFunction = innerFunction;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NestedFunction)) {
      return false;
    }
    NestedFunction o = (NestedFunction) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.innerFunction,
      o.innerFunction);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(innerFunction);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NestedFunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      innerFunction,
      other.innerFunction);
  }

  public NestedFunction withName(String name) {
    return new NestedFunction(name, innerFunction);
  }

  public NestedFunction withInnerFunction(com.gdblab.pathAlgebra.syntax.Function innerFunction) {
    return new NestedFunction(name, innerFunction);
  }
}
