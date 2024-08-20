// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class VariableEquals implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.VariableEquals");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.cypher.openCypher.Variable lhs;
  
  public final hydra.ext.cypher.openCypher.Expression rhs;
  
  public VariableEquals (hydra.ext.cypher.openCypher.Variable lhs, hydra.ext.cypher.openCypher.Expression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableEquals)) {
      return false;
    }
    VariableEquals o = (VariableEquals) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public VariableEquals withLhs(hydra.ext.cypher.openCypher.Variable lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new VariableEquals(lhs, rhs);
  }
  
  public VariableEquals withRhs(hydra.ext.cypher.openCypher.Expression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new VariableEquals(lhs, rhs);
  }
}
