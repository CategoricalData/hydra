// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * Left and right padding for an operator
 */
public class Padding implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.Padding");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ast.Ws left;
  
  public final hydra.ast.Ws right;
  
  public Padding (hydra.ast.Ws left, hydra.ast.Ws right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Padding)) {
      return false;
    }
    Padding o = (Padding) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public Padding withLeft(hydra.ast.Ws left) {
    java.util.Objects.requireNonNull((left));
    return new Padding(left, right);
  }
  
  public Padding withRight(hydra.ast.Ws right) {
    java.util.Objects.requireNonNull((right));
    return new Padding(left, right);
  }
}