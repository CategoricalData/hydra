// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class BitwiseOr implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.BitwiseOr");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.BitwiseOr> lhs;
  
  public final hydra.ext.python.syntax.BitwiseXor rhs;
  
  public BitwiseOr (hydra.util.Opt<hydra.ext.python.syntax.BitwiseOr> lhs, hydra.ext.python.syntax.BitwiseXor rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BitwiseOr)) {
      return false;
    }
    BitwiseOr o = (BitwiseOr) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public BitwiseOr withLhs(hydra.util.Opt<hydra.ext.python.syntax.BitwiseOr> lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new BitwiseOr(lhs, rhs);
  }
  
  public BitwiseOr withRhs(hydra.ext.python.syntax.BitwiseXor rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new BitwiseOr(lhs, rhs);
  }
}