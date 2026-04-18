// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class NormalApplication implements Serializable, Comparable<NormalApplication> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.NormalApplication");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.coq.syntax.Term1 lhs;

  public final java.util.List<hydra.coq.syntax.Arg> rhs;

  public NormalApplication (hydra.coq.syntax.Term1 lhs, java.util.List<hydra.coq.syntax.Arg> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalApplication)) {
      return false;
    }
    NormalApplication o = (NormalApplication) other;
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
  public int compareTo(NormalApplication other) {
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

  public NormalApplication withLhs(hydra.coq.syntax.Term1 lhs) {
    return new NormalApplication(lhs, rhs);
  }

  public NormalApplication withRhs(java.util.List<hydra.coq.syntax.Arg> rhs) {
    return new NormalApplication(lhs, rhs);
  }
}
