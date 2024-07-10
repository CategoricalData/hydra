// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class AddOrSubtractExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.AddOrSubtractExpression");
  
  public final hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression left;
  
  public final java.util.List<hydra.langs.cypher.openCypher.AddOrSubtractRightHandSide> right;
  
  public AddOrSubtractExpression (hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression left, java.util.List<hydra.langs.cypher.openCypher.AddOrSubtractRightHandSide> right) {
    if (left == null) {
      throw new IllegalArgumentException("null value for 'left' argument");
    }
    if (right == null) {
      throw new IllegalArgumentException("null value for 'right' argument");
    }
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AddOrSubtractExpression)) {
      return false;
    }
    AddOrSubtractExpression o = (AddOrSubtractExpression) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public AddOrSubtractExpression withLeft(hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression left) {
    if (left == null) {
      throw new IllegalArgumentException("null value for 'left' argument");
    }
    return new AddOrSubtractExpression(left, right);
  }
  
  public AddOrSubtractExpression withRight(java.util.List<hydra.langs.cypher.openCypher.AddOrSubtractRightHandSide> right) {
    if (right == null) {
      throw new IllegalArgumentException("null value for 'right' argument");
    }
    return new AddOrSubtractExpression(left, right);
  }
}