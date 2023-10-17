package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class VariableEquals implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.VariableEquals");
  
  public final hydra.langs.cypher.openCypher.Variable left;
  
  public final hydra.langs.cypher.openCypher.Expression right;
  
  public VariableEquals (hydra.langs.cypher.openCypher.Variable left, hydra.langs.cypher.openCypher.Expression right) {
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableEquals)) {
      return false;
    }
    VariableEquals o = (VariableEquals) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public VariableEquals withLeft(hydra.langs.cypher.openCypher.Variable left) {
    return new VariableEquals(left, right);
  }
  
  public VariableEquals withRight(hydra.langs.cypher.openCypher.Expression right) {
    return new VariableEquals(left, right);
  }
}