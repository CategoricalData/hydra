// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type which provides a choice between a 'left' type and a 'right' type
 */
public class EitherType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.EitherType");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  /**
   * The 'left' alternative
   */
  public final hydra.core.Type left;
  
  /**
   * The 'right' alternative
   */
  public final hydra.core.Type right;
  
  public EitherType (hydra.core.Type left, hydra.core.Type right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EitherType)) {
      return false;
    }
    EitherType o = (EitherType) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public EitherType withLeft(hydra.core.Type left) {
    java.util.Objects.requireNonNull((left));
    return new EitherType(left, right);
  }
  
  public EitherType withRight(hydra.core.Type right) {
    java.util.Objects.requireNonNull((right));
    return new EitherType(left, right);
  }
}
