// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class MultiplyDivideModuloExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MultiplyDivideModuloExpression");
  
  public final hydra.langs.cypher.openCypher.PowerOfExpression left;
  
  public final java.util.List<hydra.langs.cypher.openCypher.MultiplyDivideModuloRightHandSide> right;
  
  public MultiplyDivideModuloExpression (hydra.langs.cypher.openCypher.PowerOfExpression left, java.util.List<hydra.langs.cypher.openCypher.MultiplyDivideModuloRightHandSide> right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiplyDivideModuloExpression)) {
      return false;
    }
    MultiplyDivideModuloExpression o = (MultiplyDivideModuloExpression) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public MultiplyDivideModuloExpression withLeft(hydra.langs.cypher.openCypher.PowerOfExpression left) {
    java.util.Objects.requireNonNull((left));
    return new MultiplyDivideModuloExpression(left, right);
  }
  
  public MultiplyDivideModuloExpression withRight(java.util.List<hydra.langs.cypher.openCypher.MultiplyDivideModuloRightHandSide> right) {
    java.util.Objects.requireNonNull((right));
    return new MultiplyDivideModuloExpression(left, right);
  }
}