// Note: this is an automatically generated file. Do not edit.

package hydra.constraints;

import java.io.Serializable;

/**
 * A declared equivalence between two abstract paths in a graph
 */
public class PathEquation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/constraints.PathEquation");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.query.Path left;
  
  public final hydra.query.Path right;
  
  public PathEquation (hydra.query.Path left, hydra.query.Path right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathEquation)) {
      return false;
    }
    PathEquation o = (PathEquation) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public PathEquation withLeft(hydra.query.Path left) {
    java.util.Objects.requireNonNull((left));
    return new PathEquation(left, right);
  }
  
  public PathEquation withRight(hydra.query.Path right) {
    java.util.Objects.requireNonNull((right));
    return new PathEquation(left, right);
  }
}