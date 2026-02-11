// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type which provides a choice between a 'left' type and a 'right' type
 */
public class EitherType implements Serializable, Comparable<EitherType> {
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
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EitherType)) {
      return false;
    }
    EitherType o = (EitherType) other;
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
  public int compareTo(EitherType other) {
    int cmp = 0;
    cmp = ((Comparable) left).compareTo(other.left);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) right).compareTo(other.right);
  }
  
  public EitherType withLeft(hydra.core.Type left) {
    return new EitherType(left, right);
  }
  
  public EitherType withRight(hydra.core.Type right) {
    return new EitherType(left, right);
  }
}
