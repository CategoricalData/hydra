// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A comment
 */
public class Comment implements Serializable, Comparable<Comment> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.Comment");

  public static final hydra.core.Name STYLE = new hydra.core.Name("style");

  public static final hydra.core.Name TEXT = new hydra.core.Name("text");

  /**
   * The comment style
   */
  public final hydra.ext.lisp.syntax.CommentStyle style;

  /**
   * The comment text
   */
  public final String text;

  public Comment (hydra.ext.lisp.syntax.CommentStyle style, String text) {
    this.style = style;
    this.text = text;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Comment)) {
      return false;
    }
    Comment o = (Comment) other;
    return java.util.Objects.equals(
      this.style,
      o.style) && java.util.Objects.equals(
      this.text,
      o.text);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(style) + 3 * java.util.Objects.hashCode(text);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Comment other) {
    int cmp = 0;
    cmp = ((Comparable) style).compareTo(other.style);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) text).compareTo(other.text);
  }

  public Comment withStyle(hydra.ext.lisp.syntax.CommentStyle style) {
    return new Comment(style, text);
  }

  public Comment withText(String text) {
    return new Comment(style, text);
  }
}
