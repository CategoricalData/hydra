// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * A data declaration together with any comments
 */
public class DeclarationWithComments implements Serializable, Comparable<DeclarationWithComments> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.DeclarationWithComments");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name COMMENTS = new hydra.core.Name("comments");

  /**
   * The declaration
   */
  public final hydra.ext.haskell.syntax.Declaration body;

  /**
   * Optional comments
   */
  public final hydra.util.Maybe<String> comments;

  public DeclarationWithComments (hydra.ext.haskell.syntax.Declaration body, hydra.util.Maybe<String> comments) {
    this.body = body;
    this.comments = comments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DeclarationWithComments)) {
      return false;
    }
    DeclarationWithComments o = (DeclarationWithComments) other;
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
  public int compareTo(DeclarationWithComments other) {
    int cmp = 0;
    cmp = ((Comparable) body).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) comments).compareTo(other.comments);
  }

  public DeclarationWithComments withBody(hydra.ext.haskell.syntax.Declaration body) {
    return new DeclarationWithComments(body, comments);
  }

  public DeclarationWithComments withComments(hydra.util.Maybe<String> comments) {
    return new DeclarationWithComments(body, comments);
  }
}
