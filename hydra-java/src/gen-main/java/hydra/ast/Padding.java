// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * Left and right padding for an operator
 */
public class Padding implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ast.Padding");
  
  public final hydra.ast.Ws left;
  
  public final hydra.ast.Ws right;
  
  public Padding (hydra.ast.Ws left, hydra.ast.Ws right) {
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
    if (left == null) {
      throw new IllegalArgumentException("null value for 'left' argument");
    }
    return new Padding(left, right);
  }
  
  public Padding withRight(hydra.ast.Ws right) {
    if (right == null) {
      throw new IllegalArgumentException("null value for 'right' argument");
    }
    return new Padding(left, right);
  }
}