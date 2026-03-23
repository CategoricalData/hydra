// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Alternative implements Serializable, Comparable<Pat_Alternative> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Pat_Alternative");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.ext.scala.meta.Pat lhs;

  public final hydra.ext.scala.meta.Pat rhs;

  public Pat_Alternative (hydra.ext.scala.meta.Pat lhs, hydra.ext.scala.meta.Pat rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Alternative)) {
      return false;
    }
    Pat_Alternative o = (Pat_Alternative) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(rhs);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pat_Alternative other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }

  public Pat_Alternative withLhs(hydra.ext.scala.meta.Pat lhs) {
    return new Pat_Alternative(lhs, rhs);
  }

  public Pat_Alternative withRhs(hydra.ext.scala.meta.Pat rhs) {
    return new Pat_Alternative(lhs, rhs);
  }
}
