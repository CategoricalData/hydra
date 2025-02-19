// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class PrimaryWithRhs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.PrimaryWithRhs");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.Primary primary;
  
  public final hydra.ext.python.syntax.PrimaryRhs rhs;
  
  public PrimaryWithRhs (hydra.ext.python.syntax.Primary primary, hydra.ext.python.syntax.PrimaryRhs rhs) {
    java.util.Objects.requireNonNull((primary));
    java.util.Objects.requireNonNull((rhs));
    this.primary = primary;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimaryWithRhs)) {
      return false;
    }
    PrimaryWithRhs o = (PrimaryWithRhs) (other);
    return primary.equals(o.primary) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * rhs.hashCode();
  }
  
  public PrimaryWithRhs withPrimary(hydra.ext.python.syntax.Primary primary) {
    java.util.Objects.requireNonNull((primary));
    return new PrimaryWithRhs(primary, rhs);
  }
  
  public PrimaryWithRhs withRhs(hydra.ext.python.syntax.PrimaryRhs rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new PrimaryWithRhs(primary, rhs);
  }
}