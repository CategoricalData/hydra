// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class VariablePlusEquals implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.VariablePlusEquals");
  
  public final hydra.langs.cypher.openCypher.Variable lhs;
  
  public final hydra.langs.cypher.openCypher.Expression rhs;
  
  public VariablePlusEquals (hydra.langs.cypher.openCypher.Variable lhs, hydra.langs.cypher.openCypher.Expression rhs) {
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
  
  public VariablePlusEquals withLhs(hydra.langs.cypher.openCypher.Variable lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new VariablePlusEquals(lhs, rhs);
  }
  
  public VariablePlusEquals withRhs(hydra.langs.cypher.openCypher.Expression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new VariablePlusEquals(lhs, rhs);
  }
}