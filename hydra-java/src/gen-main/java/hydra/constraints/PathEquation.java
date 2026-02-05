// Note: this is an automatically generated file. Do not edit.

package hydra.constraints;

import java.io.Serializable;

/**
 * A declared equivalence between two abstract paths in a graph
 */
public class PathEquation implements Serializable, Comparable<PathEquation> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.constraints.PathEquation");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  /**
   * The left-hand side of the equation
   */
  public final hydra.query.Path left;
  
  /**
   * The right-hand side of the equation
   */
  public final hydra.query.Path right;
  
  public PathEquation (hydra.query.Path left, hydra.query.Path right) {
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathEquation)) {
      return false;
    }
    PathEquation o = (PathEquation) (other);
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
  public int compareTo(PathEquation other) {
    int cmp = 0;
    cmp = ((Comparable) (left)).compareTo(other.left);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (right)).compareTo(other.right);
  }
  
  public PathEquation withLeft(hydra.query.Path left) {
    return new PathEquation(left, right);
  }
  
  public PathEquation withRight(hydra.query.Path right) {
    return new PathEquation(left, right);
  }
}
