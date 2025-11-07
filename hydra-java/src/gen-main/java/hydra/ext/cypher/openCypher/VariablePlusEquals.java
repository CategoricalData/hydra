// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class VariablePlusEquals implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.VariablePlusEquals");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.cypher.openCypher.Variable lhs;
  
  public final hydra.ext.cypher.openCypher.Expression rhs;
  
  public VariablePlusEquals (hydra.ext.cypher.openCypher.Variable lhs, hydra.ext.cypher.openCypher.Expression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariablePlusEquals)) {
      return false;
    }
    VariablePlusEquals o = (VariablePlusEquals) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public VariablePlusEquals withLhs(hydra.ext.cypher.openCypher.Variable lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new VariablePlusEquals(lhs, rhs);
  }
  
  public VariablePlusEquals withRhs(hydra.ext.cypher.openCypher.Expression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new VariablePlusEquals(lhs, rhs);
  }
}
