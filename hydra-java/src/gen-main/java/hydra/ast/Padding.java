// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * Left and right padding for an operator
 */
public class Padding implements Serializable, Comparable<Padding> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.Padding");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  /**
   * Padding to the left of the operator
   */
  public final hydra.ast.Ws left;
  
  /**
   * Padding to the right of the operator
   */
  public final hydra.ast.Ws right;
  
  public Padding (hydra.ast.Ws left, hydra.ast.Ws right) {
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Padding)) {
      return false;
    }
    Padding o = (Padding) (other);
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Padding other) {
    int cmp = 0;
    cmp = ((Comparable) (left)).compareTo(other.left);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (right)).compareTo(other.right);
  }
  
  public Padding withLeft(hydra.ast.Ws left) {
    return new Padding(left, right);
  }
  
  public Padding withRight(hydra.ast.Ws right) {
    return new Padding(left, right);
  }
}
