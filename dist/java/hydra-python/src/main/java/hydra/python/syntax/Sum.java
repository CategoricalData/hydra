// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class Sum implements Serializable, Comparable<Sum> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.Sum");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.util.Maybe<hydra.python.syntax.SumLhs> lhs;

  public final hydra.python.syntax.Term rhs;

  public Sum (hydra.util.Maybe<hydra.python.syntax.SumLhs> lhs, hydra.python.syntax.Term rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Sum)) {
      return false;
    }
    Sum o = (Sum) other;
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
  public int compareTo(Sum other) {
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

  public Sum withLhs(hydra.util.Maybe<hydra.python.syntax.SumLhs> lhs) {
    return new Sum(lhs, rhs);
  }

  public Sum withRhs(hydra.python.syntax.Term rhs) {
    return new Sum(lhs, rhs);
  }
}
