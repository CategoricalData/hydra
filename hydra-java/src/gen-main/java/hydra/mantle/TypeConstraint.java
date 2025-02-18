// Note: this is an automatically generated file. Do not edit.

package hydra.mantle;

import java.io.Serializable;

/**
 * An assertion that two types can be unified into a single type
 */
public class TypeConstraint implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/mantle.TypeConstraint");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public final hydra.core.Type left;
  
  public final hydra.core.Type right;
  
  /**
   * An optional description of the type constraint. This may be used for tracing or debugging.
   */
  public final hydra.util.Opt<String> comment;
  
  public TypeConstraint (hydra.core.Type left, hydra.core.Type right, hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    java.util.Objects.requireNonNull((comment));
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
    return left.equals(o.left) && right.equals(o.right) && comment.equals(o.comment);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode() + 5 * comment.hashCode();
  }
  
  public TypeConstraint withLeft(hydra.core.Type left) {
    java.util.Objects.requireNonNull((left));
    return new TypeConstraint(left, right, comment);
  }
  
  public TypeConstraint withRight(hydra.core.Type right) {
    java.util.Objects.requireNonNull((right));
    return new TypeConstraint(left, right, comment);
  }
  
  public TypeConstraint withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new TypeConstraint(left, right, comment);
  }
}