// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class PrimaryWithRhs implements Serializable, Comparable<PrimaryWithRhs> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.PrimaryWithRhs");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.Primary primary;
  
  public final hydra.ext.python.syntax.PrimaryRhs rhs;
  
  public PrimaryWithRhs (hydra.ext.python.syntax.Primary primary, hydra.ext.python.syntax.PrimaryRhs rhs) {
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
    cmp = ((Comparable) primary).compareTo(other.primary);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public PrimaryWithRhs withPrimary(hydra.ext.python.syntax.Primary primary) {
    return new PrimaryWithRhs(primary, rhs);
  }
  
  public PrimaryWithRhs withRhs(hydra.ext.python.syntax.PrimaryRhs rhs) {
    return new PrimaryWithRhs(primary, rhs);
  }
}
