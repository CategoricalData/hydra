// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class PrimaryWithRhs implements Serializable, Comparable<PrimaryWithRhs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.PrimaryWithRhs");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.python.syntax.Primary primary;

  public final hydra.python.syntax.PrimaryRhs rhs;

  public PrimaryWithRhs (hydra.python.syntax.Primary primary, hydra.python.syntax.PrimaryRhs rhs) {
    this.primary = primary;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimaryWithRhs)) {
      return false;
    }
    PrimaryWithRhs o = (PrimaryWithRhs) other;
    return java.util.Objects.equals(
      this.primary,
      o.primary) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(primary) + 3 * java.util.Objects.hashCode(rhs);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PrimaryWithRhs other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      primary,
      other.primary);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rhs,
      other.rhs);
  }

  public PrimaryWithRhs withPrimary(hydra.python.syntax.Primary primary) {
    return new PrimaryWithRhs(primary, rhs);
  }

  public PrimaryWithRhs withRhs(hydra.python.syntax.PrimaryRhs rhs) {
    return new PrimaryWithRhs(primary, rhs);
  }
}
