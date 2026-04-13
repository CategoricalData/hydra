// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class BitwiseOr implements Serializable, Comparable<BitwiseOr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.BitwiseOr");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.util.Maybe<hydra.python.syntax.BitwiseOr> lhs;

  public final hydra.python.syntax.BitwiseXor rhs;

  public BitwiseOr (hydra.util.Maybe<hydra.python.syntax.BitwiseOr> lhs, hydra.python.syntax.BitwiseXor rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BitwiseOr)) {
      return false;
    }
    BitwiseOr o = (BitwiseOr) other;
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
  public int compareTo(BitwiseOr other) {
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

  public BitwiseOr withLhs(hydra.util.Maybe<hydra.python.syntax.BitwiseOr> lhs) {
    return new BitwiseOr(lhs, rhs);
  }

  public BitwiseOr withRhs(hydra.python.syntax.BitwiseXor rhs) {
    return new BitwiseOr(lhs, rhs);
  }
}
