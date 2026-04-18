// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class Concatenation implements Serializable, Comparable<Concatenation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.Concatenation");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final com.gdblab.pathAlgebra.syntax.Rpq left;

  public final com.gdblab.pathAlgebra.syntax.Rpq right;

  public Concatenation (com.gdblab.pathAlgebra.syntax.Rpq left, com.gdblab.pathAlgebra.syntax.Rpq right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Concatenation)) {
      return false;
    }
    Concatenation o = (Concatenation) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Concatenation other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      left,
      other.left);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      right,
      other.right);
  }

  public Concatenation withLeft(com.gdblab.pathAlgebra.syntax.Rpq left) {
    return new Concatenation(left, right);
  }

  public Concatenation withRight(com.gdblab.pathAlgebra.syntax.Rpq right) {
    return new Concatenation(left, right);
  }
}
