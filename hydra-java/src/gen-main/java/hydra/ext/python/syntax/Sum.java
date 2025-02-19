// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Sum implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Sum");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.SumLhs> lhs;
  
  public final hydra.ext.python.syntax.Term rhs;
  
  public Sum (hydra.util.Opt<hydra.ext.python.syntax.SumLhs> lhs, hydra.ext.python.syntax.Term rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Sum)) {
      return false;
    }
    Sum o = (Sum) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Sum withLhs(hydra.util.Opt<hydra.ext.python.syntax.SumLhs> lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Sum(lhs, rhs);
  }
  
  public Sum withRhs(hydra.ext.python.syntax.Term rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Sum(lhs, rhs);
  }
}