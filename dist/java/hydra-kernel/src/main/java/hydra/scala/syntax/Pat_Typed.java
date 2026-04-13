// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Pat_Typed implements Serializable, Comparable<Pat_Typed> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Pat_Typed");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.scala.syntax.Pat lhs;

  public final hydra.scala.syntax.Type rhs;

  public Pat_Typed (hydra.scala.syntax.Pat lhs, hydra.scala.syntax.Type rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Typed)) {
      return false;
    }
    Pat_Typed o = (Pat_Typed) other;
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
  public int compareTo(Pat_Typed other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      lhs,
      other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rhs,
      other.rhs);
  }

  public Pat_Typed withLhs(hydra.scala.syntax.Pat lhs) {
    return new Pat_Typed(lhs, rhs);
  }

  public Pat_Typed withRhs(hydra.scala.syntax.Type rhs) {
    return new Pat_Typed(lhs, rhs);
  }
}
