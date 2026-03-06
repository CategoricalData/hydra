// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class VariableEquals implements Serializable, Comparable<VariableEquals> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.VariableEquals");
  
  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.cypher.openCypher.Variable lhs;
  
  public final hydra.ext.cypher.openCypher.Expression rhs;
  
  public VariableEquals (hydra.ext.cypher.openCypher.Variable lhs, hydra.ext.cypher.openCypher.Expression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableEquals)) {
      return false;
    }
    VariableEquals o = (VariableEquals) other;
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
  public int compareTo(VariableEquals other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public VariableEquals withLhs(hydra.ext.cypher.openCypher.Variable lhs) {
    return new VariableEquals(lhs, rhs);
  }
  
  public VariableEquals withRhs(hydra.ext.cypher.openCypher.Expression rhs) {
    return new VariableEquals(lhs, rhs);
  }
}
