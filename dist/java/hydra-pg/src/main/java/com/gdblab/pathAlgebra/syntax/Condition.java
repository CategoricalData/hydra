// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class Condition implements Serializable, Comparable<Condition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.Condition");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name COMPARE_SYM = new hydra.core.Name("compareSym");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final com.gdblab.pathAlgebra.syntax.Function function;

  public final com.gdblab.pathAlgebra.syntax.CompareSym compareSym;

  public final String value;

  public Condition (com.gdblab.pathAlgebra.syntax.Function function, com.gdblab.pathAlgebra.syntax.CompareSym compareSym, String value) {
    this.function = function;
    this.compareSym = compareSym;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Condition)) {
      return false;
    }
    Condition o = (Condition) other;
    return java.util.Objects.equals(
      this.function,
      o.function) && java.util.Objects.equals(
      this.compareSym,
      o.compareSym) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(function) + 3 * java.util.Objects.hashCode(compareSym) + 5 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Condition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      function,
      other.function);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      compareSym,
      other.compareSym);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public Condition withFunction(com.gdblab.pathAlgebra.syntax.Function function) {
    return new Condition(function, compareSym, value);
  }

  public Condition withCompareSym(com.gdblab.pathAlgebra.syntax.CompareSym compareSym) {
    return new Condition(function, compareSym, value);
  }

  public Condition withValue(String value) {
    return new Condition(function, compareSym, value);
  }
}
