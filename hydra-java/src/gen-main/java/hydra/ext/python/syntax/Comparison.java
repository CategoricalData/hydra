// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Comparison implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Comparison");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.BitwiseOr lhs;
  
  public final java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair> rhs;
  
  public Comparison (hydra.ext.python.syntax.BitwiseOr lhs, java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair> rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Comparison)) {
      return false;
    }
    Comparison o = (Comparison) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Comparison withLhs(hydra.ext.python.syntax.BitwiseOr lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Comparison(lhs, rhs);
  }
  
  public Comparison withRhs(java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair> rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Comparison(lhs, rhs);
  }
}