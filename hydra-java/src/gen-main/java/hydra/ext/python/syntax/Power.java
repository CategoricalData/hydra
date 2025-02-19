// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Power implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Power");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.AwaitPrimary lhs;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Factor> rhs;
  
  public Power (hydra.ext.python.syntax.AwaitPrimary lhs, hydra.util.Opt<hydra.ext.python.syntax.Factor> rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Power)) {
      return false;
    }
    Power o = (Power) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Power withLhs(hydra.ext.python.syntax.AwaitPrimary lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Power(lhs, rhs);
  }
  
  public Power withRhs(hydra.util.Opt<hydra.ext.python.syntax.Factor> rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Power(lhs, rhs);
  }
}