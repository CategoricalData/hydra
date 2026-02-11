// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A field together with any comments
 */
public class FieldWithComments implements Serializable, Comparable<FieldWithComments> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.FieldWithComments");
  
  public static final hydra.core.Name FIELD_NAME_FIELD = new hydra.core.Name("field");
  
  public static final hydra.core.Name FIELD_NAME_COMMENTS = new hydra.core.Name("comments");
  
  /**
   * The field
   */
  public final hydra.ext.haskell.ast.Field field;
  
  /**
   * Optional comments
   */
  public final hydra.util.Maybe<String> comments;
  
  public FieldWithComments (hydra.ext.haskell.ast.Field field, hydra.util.Maybe<String> comments) {
    this.field = field;
    this.comments = comments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldWithComments)) {
      return false;
    }
    FieldWithComments o = (FieldWithComments) other;
    return java.util.Objects.equals(
      this.field,
      o.field) && java.util.Objects.equals(
      this.comments,
      o.comments);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(field) + 3 * java.util.Objects.hashCode(comments);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FieldWithComments other) {
    int cmp = 0;
    cmp = ((Comparable) field).compareTo(other.field);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      comments.hashCode(),
      other.comments.hashCode());
  }
  
  public FieldWithComments withField(hydra.ext.haskell.ast.Field field) {
    return new FieldWithComments(field, comments);
  }
  
  public FieldWithComments withComments(hydra.util.Maybe<String> comments) {
    return new FieldWithComments(field, comments);
  }
}
