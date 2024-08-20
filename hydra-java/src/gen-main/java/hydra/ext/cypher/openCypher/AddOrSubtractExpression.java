// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class AddOrSubtractExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.AddOrSubtractExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.cypher.openCypher.MultiplyDivideModuloExpression left;
  
  public final java.util.List<hydra.ext.cypher.openCypher.AddOrSubtractRightHandSide> right;
  
  public AddOrSubtractExpression (hydra.ext.cypher.openCypher.MultiplyDivideModuloExpression left, java.util.List<hydra.ext.cypher.openCypher.AddOrSubtractRightHandSide> right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
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
  
  public AddOrSubtractExpression withLeft(hydra.ext.cypher.openCypher.MultiplyDivideModuloExpression left) {
    java.util.Objects.requireNonNull((left));
    return new AddOrSubtractExpression(left, right);
  }
  
  public AddOrSubtractExpression withRight(java.util.List<hydra.ext.cypher.openCypher.AddOrSubtractRightHandSide> right) {
    java.util.Objects.requireNonNull((right));
    return new AddOrSubtractExpression(left, right);
  }
}
