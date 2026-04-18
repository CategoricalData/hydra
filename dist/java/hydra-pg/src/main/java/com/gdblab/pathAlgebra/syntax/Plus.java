// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class Plus implements Serializable, Comparable<Plus> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.Plus");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name RESTRICTOR = new hydra.core.Name("restrictor");

  public final com.gdblab.pathAlgebra.syntax.Rpq expression;

  public final hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.RpqRestrictor> restrictor;

  public Plus (com.gdblab.pathAlgebra.syntax.Rpq expression, hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.RpqRestrictor> restrictor) {
    this.expression = expression;
    this.restrictor = restrictor;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Plus)) {
      return false;
    }
    Plus o = (Plus) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.restrictor,
      o.restrictor);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(restrictor);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Plus other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      restrictor,
      other.restrictor);
  }

  public Plus withExpression(com.gdblab.pathAlgebra.syntax.Rpq expression) {
    return new Plus(expression, restrictor);
  }

  public Plus withRestrictor(hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.RpqRestrictor> restrictor) {
    return new Plus(expression, restrictor);
  }
}
