// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Comparison implements Serializable, Comparable<Comparison> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Comparison");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.BitwiseOr lhs;
  
  public final java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair> rhs;
  
  public Comparison (hydra.ext.python.syntax.BitwiseOr lhs, java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Comparison)) {
      return false;
    }
    Comparison o = (Comparison) other;
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
  public int compareTo(Comparison other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      rhs.hashCode(),
      other.rhs.hashCode());
  }
  
  public Comparison withLhs(hydra.ext.python.syntax.BitwiseOr lhs) {
    return new Comparison(lhs, rhs);
  }
  
  public Comparison withRhs(java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair> rhs) {
    return new Comparison(lhs, rhs);
  }
}
