// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class MultiplyDivideModuloExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.MultiplyDivideModuloExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.cypher.openCypher.PowerOfExpression left;
  
  public final java.util.List<hydra.ext.cypher.openCypher.MultiplyDivideModuloRightHandSide> right;
  
  public MultiplyDivideModuloExpression (hydra.ext.cypher.openCypher.PowerOfExpression left, java.util.List<hydra.ext.cypher.openCypher.MultiplyDivideModuloRightHandSide> right) {
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
  
  public MultiplyDivideModuloExpression withLeft(hydra.ext.cypher.openCypher.PowerOfExpression left) {
    java.util.Objects.requireNonNull((left));
    return new MultiplyDivideModuloExpression(left, right);
  }
  
  public MultiplyDivideModuloExpression withRight(java.util.List<hydra.ext.cypher.openCypher.MultiplyDivideModuloRightHandSide> right) {
    java.util.Objects.requireNonNull((right));
    return new MultiplyDivideModuloExpression(left, right);
  }
}