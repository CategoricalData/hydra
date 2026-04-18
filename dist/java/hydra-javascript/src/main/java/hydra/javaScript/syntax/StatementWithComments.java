// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A statement with optional documentation
 */
public class StatementWithComments implements Serializable, Comparable<StatementWithComments> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.StatementWithComments");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name COMMENTS = new hydra.core.Name("comments");

  /**
   * The statement
   */
  public final hydra.javaScript.syntax.Statement body;

  /**
   * Optional documentation comment
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments;

  public StatementWithComments (hydra.javaScript.syntax.Statement body, hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments) {
    this.body = body;
    this.comments = comments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StatementWithComments)) {
      return false;
    }
    StatementWithComments o = (StatementWithComments) other;
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
  public int compareTo(StatementWithComments other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      comments,
      other.comments);
  }

  public StatementWithComments withBody(hydra.javaScript.syntax.Statement body) {
    return new StatementWithComments(body, comments);
  }

  public StatementWithComments withComments(hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments) {
    return new StatementWithComments(body, comments);
  }
}
