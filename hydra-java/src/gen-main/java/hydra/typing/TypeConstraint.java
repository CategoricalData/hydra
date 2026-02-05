// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * An assertion that two types can be unified into a single type
 */
public class TypeConstraint implements Serializable, Comparable<TypeConstraint> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.typing.TypeConstraint");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  /**
   * The left-hand side of the constraint
   */
  public final hydra.core.Type left;
  
  /**
   * The right-hand side of the constraint
   */
  public final hydra.core.Type right;
  
  /**
   * A description of the type constraint which may be used for tracing or debugging
   */
  public final String comment;
  
  public TypeConstraint (hydra.core.Type left, hydra.core.Type right, String comment) {
    this.left = left;
    this.right = right;
    this.comment = comment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeConstraint)) {
      return false;
    }
    TypeConstraint o = (TypeConstraint) (other);
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right) && java.util.Objects.equals(
      this.comment,
      o.comment);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right) + 5 * java.util.Objects.hashCode(comment);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeConstraint other) {
    int cmp = 0;
    cmp = ((Comparable) (left)).compareTo(other.left);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (right)).compareTo(other.right);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (comment)).compareTo(other.comment);
  }
  
  public TypeConstraint withLeft(hydra.core.Type left) {
    return new TypeConstraint(left, right, comment);
  }
  
  public TypeConstraint withRight(hydra.core.Type right) {
    return new TypeConstraint(left, right, comment);
  }
  
  public TypeConstraint withComment(String comment) {
    return new TypeConstraint(left, right, comment);
  }
}
