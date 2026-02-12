// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class BitwiseAnd implements Serializable, Comparable<BitwiseAnd> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.BitwiseAnd");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd> lhs;
  
  public final hydra.ext.python.syntax.ShiftExpression rhs;
  
  public BitwiseAnd (hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd> lhs, hydra.ext.python.syntax.ShiftExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BitwiseAnd)) {
      return false;
    }
    BitwiseAnd o = (BitwiseAnd) other;
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
  public int compareTo(BitwiseAnd other) {
    int cmp = 0;
    cmp = Integer.compare(
      lhs.hashCode(),
      other.lhs.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public BitwiseAnd withLhs(hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd> lhs) {
    return new BitwiseAnd(lhs, rhs);
  }
  
  public BitwiseAnd withRhs(hydra.ext.python.syntax.ShiftExpression rhs) {
    return new BitwiseAnd(lhs, rhs);
  }
}
