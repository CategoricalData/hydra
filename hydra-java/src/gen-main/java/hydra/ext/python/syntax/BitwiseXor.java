// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class BitwiseXor implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.BitwiseXor");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.BitwiseXor> lhs;
  
  public final hydra.ext.python.syntax.BitwiseAnd rhs;
  
  public BitwiseXor (hydra.util.Opt<hydra.ext.python.syntax.BitwiseXor> lhs, hydra.ext.python.syntax.BitwiseAnd rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BitwiseXor)) {
      return false;
    }
    BitwiseXor o = (BitwiseXor) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public BitwiseXor withLhs(hydra.util.Opt<hydra.ext.python.syntax.BitwiseXor> lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new BitwiseXor(lhs, rhs);
  }
  
  public BitwiseXor withRhs(hydra.ext.python.syntax.BitwiseAnd rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new BitwiseXor(lhs, rhs);
  }
}