// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ShiftExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ShiftExpression");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.ShiftLhs> lhs;
  
  public final hydra.ext.python.syntax.Sum rhs;
  
  public ShiftExpression (hydra.util.Opt<hydra.ext.python.syntax.ShiftLhs> lhs, hydra.ext.python.syntax.Sum rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShiftExpression)) {
      return false;
    }
    ShiftExpression o = (ShiftExpression) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public ShiftExpression withLhs(hydra.util.Opt<hydra.ext.python.syntax.ShiftLhs> lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new ShiftExpression(lhs, rhs);
  }
  
  public ShiftExpression withRhs(hydra.ext.python.syntax.Sum rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new ShiftExpression(lhs, rhs);
  }
}