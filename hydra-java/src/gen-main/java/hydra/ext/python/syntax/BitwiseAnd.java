// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class BitwiseAnd implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.BitwiseAnd");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.BitwiseAnd> lhs;
  
  public final hydra.ext.python.syntax.ShiftExpression rhs;
  
  public BitwiseAnd (hydra.util.Opt<hydra.ext.python.syntax.BitwiseAnd> lhs, hydra.ext.python.syntax.ShiftExpression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BitwiseAnd)) {
      return false;
    }
    BitwiseAnd o = (BitwiseAnd) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public BitwiseAnd withLhs(hydra.util.Opt<hydra.ext.python.syntax.BitwiseAnd> lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new BitwiseAnd(lhs, rhs);
  }
  
  public BitwiseAnd withRhs(hydra.ext.python.syntax.ShiftExpression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new BitwiseAnd(lhs, rhs);
  }
}