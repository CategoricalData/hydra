// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Sum implements Serializable, Comparable<Sum> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Sum");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.SumLhs> lhs;
  
  public final hydra.ext.python.syntax.Term rhs;
  
  public Sum (hydra.util.Maybe<hydra.ext.python.syntax.SumLhs> lhs, hydra.ext.python.syntax.Term rhs) {
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
    cmp = Integer.compare(
      lhs.hashCode(),
      other.lhs.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public Sum withLhs(hydra.util.Maybe<hydra.ext.python.syntax.SumLhs> lhs) {
    return new Sum(lhs, rhs);
  }
  
  public Sum withRhs(hydra.ext.python.syntax.Term rhs) {
    return new Sum(lhs, rhs);
  }
}
