// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Power implements Serializable, Comparable<Power> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Power");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.AwaitPrimary lhs;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Factor> rhs;
  
  public Power (hydra.ext.python.syntax.AwaitPrimary lhs, hydra.util.Maybe<hydra.ext.python.syntax.Factor> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Power)) {
      return false;
    }
    Power o = (Power) other;
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
  public int compareTo(Power other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      rhs.hashCode(),
      other.rhs.hashCode());
  }
  
  public Power withLhs(hydra.ext.python.syntax.AwaitPrimary lhs) {
    return new Power(lhs, rhs);
  }
  
  public Power withRhs(hydra.util.Maybe<hydra.ext.python.syntax.Factor> rhs) {
    return new Power(lhs, rhs);
  }
}
