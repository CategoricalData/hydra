package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class AddOrSubtractExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.AddOrSubtractExpression");
  
  public final hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression lhs;
  
  public final java.util.List<hydra.langs.cypher.openCypher.AddOrSubtractRhs> rhs;
  
  public AddOrSubtractExpression (hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression lhs, java.util.List<hydra.langs.cypher.openCypher.AddOrSubtractRhs> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AddOrSubtractExpression)) {
      return false;
    }
    AddOrSubtractExpression o = (AddOrSubtractExpression) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public AddOrSubtractExpression withLhs(hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression lhs) {
    return new AddOrSubtractExpression(lhs, rhs);
  }
  
  public AddOrSubtractExpression withRhs(java.util.List<hydra.langs.cypher.openCypher.AddOrSubtractRhs> rhs) {
    return new AddOrSubtractExpression(lhs, rhs);
  }
}