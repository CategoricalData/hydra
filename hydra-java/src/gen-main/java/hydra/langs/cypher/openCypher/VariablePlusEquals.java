package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class VariablePlusEquals implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.VariablePlusEquals");
  
  public final hydra.langs.cypher.openCypher.Variable left;
  
  public final hydra.langs.cypher.openCypher.Expression right;
  
  public VariablePlusEquals (hydra.langs.cypher.openCypher.Variable left, hydra.langs.cypher.openCypher.Expression right) {
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariablePlusEquals)) {
      return false;
    }
    VariablePlusEquals o = (VariablePlusEquals) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public VariablePlusEquals withLeft(hydra.langs.cypher.openCypher.Variable left) {
    return new VariablePlusEquals(left, right);
  }
  
  public VariablePlusEquals withRight(hydra.langs.cypher.openCypher.Expression right) {
    return new VariablePlusEquals(left, right);
  }
}