package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class VariableEquals implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.VariableEquals");
  
  public final hydra.langs.cypher.openCypher.Variable lhs;
  
  public final hydra.langs.cypher.openCypher.Expression rhs;
  
  public VariableEquals (hydra.langs.cypher.openCypher.Variable lhs, hydra.langs.cypher.openCypher.Expression rhs) {
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
  
  public VariableEquals withLhs(hydra.langs.cypher.openCypher.Variable lhs) {
    return new VariableEquals(lhs, rhs);
  }
  
  public VariableEquals withRhs(hydra.langs.cypher.openCypher.Expression rhs) {
    return new VariableEquals(lhs, rhs);
  }
}