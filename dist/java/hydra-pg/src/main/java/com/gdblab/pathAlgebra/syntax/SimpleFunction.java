// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class SimpleFunction implements Serializable, Comparable<SimpleFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.SimpleFunction");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name ARGUMENT = new hydra.core.Name("argument");

  public final String name;

  public final String argument;

  public SimpleFunction (String name, String argument) {
    this.name = name;
    this.argument = argument;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleFunction)) {
      return false;
    }
    SimpleFunction o = (SimpleFunction) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.argument,
      o.argument);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(argument);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SimpleFunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      argument,
      other.argument);
  }

  public SimpleFunction withName(String name) {
    return new SimpleFunction(name, argument);
  }

  public SimpleFunction withArgument(String argument) {
    return new SimpleFunction(name, argument);
  }
}
