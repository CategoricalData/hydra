// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A data constructor together with any comments
 */
public class ConstructorWithComments implements Serializable, Comparable<ConstructorWithComments> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ConstructorWithComments");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_COMMENTS = new hydra.core.Name("comments");
  
  /**
   * The constructor
   */
  public final hydra.ext.haskell.ast.Constructor body;
  
  /**
   * Optional comments
   */
  public final hydra.util.Maybe<String> comments;
  
  public ConstructorWithComments (hydra.ext.haskell.ast.Constructor body, hydra.util.Maybe<String> comments) {
    this.body = body;
    this.comments = comments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructorWithComments)) {
      return false;
    }
    ConstructorWithComments o = (ConstructorWithComments) (other);
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.comments,
      o.comments);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(comments);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConstructorWithComments other) {
    int cmp = 0;
    cmp = ((Comparable) (body)).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      comments.hashCode(),
      other.comments.hashCode());
  }
  
  public ConstructorWithComments withBody(hydra.ext.haskell.ast.Constructor body) {
    return new ConstructorWithComments(body, comments);
  }
  
  public ConstructorWithComments withComments(hydra.util.Maybe<String> comments) {
    return new ConstructorWithComments(body, comments);
  }
}
