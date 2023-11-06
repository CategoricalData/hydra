package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class MultiplyDivideModuloExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MultiplyDivideModuloExpression");
  
  public final hydra.langs.cypher.openCypher.PowerOfExpression lhs;
  
  public final java.util.List<hydra.langs.cypher.openCypher.MultiplyDivideModuloRhs> rhs;
  
  public MultiplyDivideModuloExpression (hydra.langs.cypher.openCypher.PowerOfExpression lhs, java.util.List<hydra.langs.cypher.openCypher.MultiplyDivideModuloRhs> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiplyDivideModuloExpression)) {
      return false;
    }
    MultiplyDivideModuloExpression o = (MultiplyDivideModuloExpression) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public MultiplyDivideModuloExpression withLhs(hydra.langs.cypher.openCypher.PowerOfExpression lhs) {
    return new MultiplyDivideModuloExpression(lhs, rhs);
  }
  
  public MultiplyDivideModuloExpression withRhs(java.util.List<hydra.langs.cypher.openCypher.MultiplyDivideModuloRhs> rhs) {
    return new MultiplyDivideModuloExpression(lhs, rhs);
  }
}