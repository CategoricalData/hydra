// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ShiftExpression implements Serializable, Comparable<ShiftExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ShiftExpression");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs> lhs;
  
  public final hydra.ext.python.syntax.Sum rhs;
  
  public ShiftExpression (hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs> lhs, hydra.ext.python.syntax.Sum rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShiftExpression)) {
      return false;
    }
    ShiftExpression o = (ShiftExpression) other;
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
  public int compareTo(ShiftExpression other) {
    int cmp = 0;
    cmp = Integer.compare(
      lhs.hashCode(),
      other.lhs.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public ShiftExpression withLhs(hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs> lhs) {
    return new ShiftExpression(lhs, rhs);
  }
  
  public ShiftExpression withRhs(hydra.ext.python.syntax.Sum rhs) {
    return new ShiftExpression(lhs, rhs);
  }
}
