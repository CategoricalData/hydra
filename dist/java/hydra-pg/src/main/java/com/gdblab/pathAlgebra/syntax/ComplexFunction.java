// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class ComplexFunction implements Serializable, Comparable<ComplexFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.ComplexFunction");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name INNER_FUNCTION = new hydra.core.Name("innerFunction");

  public static final hydra.core.Name ADDITIONAL_ARG = new hydra.core.Name("additionalArg");

  public final String name;

  public final com.gdblab.pathAlgebra.syntax.Function innerFunction;

  public final String additionalArg;

  public ComplexFunction (String name, com.gdblab.pathAlgebra.syntax.Function innerFunction, String additionalArg) {
    this.name = name;
    this.innerFunction = innerFunction;
    this.additionalArg = additionalArg;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ComplexFunction)) {
      return false;
    }
    ComplexFunction o = (ComplexFunction) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.innerFunction,
      o.innerFunction) && java.util.Objects.equals(
      this.additionalArg,
      o.additionalArg);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(innerFunction) + 5 * java.util.Objects.hashCode(additionalArg);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ComplexFunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      innerFunction,
      other.innerFunction);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      additionalArg,
      other.additionalArg);
  }

  public ComplexFunction withName(String name) {
    return new ComplexFunction(name, innerFunction, additionalArg);
  }

  public ComplexFunction withInnerFunction(com.gdblab.pathAlgebra.syntax.Function innerFunction) {
    return new ComplexFunction(name, innerFunction, additionalArg);
  }

  public ComplexFunction withAdditionalArg(String additionalArg) {
    return new ComplexFunction(name, innerFunction, additionalArg);
  }
}
