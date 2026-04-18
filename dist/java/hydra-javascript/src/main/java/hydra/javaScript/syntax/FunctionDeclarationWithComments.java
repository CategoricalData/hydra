// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A function declaration with optional JSDoc
 */
public class FunctionDeclarationWithComments implements Serializable, Comparable<FunctionDeclarationWithComments> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.FunctionDeclarationWithComments");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name COMMENTS = new hydra.core.Name("comments");

  /**
   * The function declaration
   */
  public final hydra.javaScript.syntax.FunctionDeclaration body;

  /**
   * Optional JSDoc comment
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments;

  public FunctionDeclarationWithComments (hydra.javaScript.syntax.FunctionDeclaration body, hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments) {
    this.body = body;
    this.comments = comments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionDeclarationWithComments)) {
      return false;
    }
    FunctionDeclarationWithComments o = (FunctionDeclarationWithComments) other;
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
  public int compareTo(FunctionDeclarationWithComments other) {
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

  public FunctionDeclarationWithComments withBody(hydra.javaScript.syntax.FunctionDeclaration body) {
    return new FunctionDeclarationWithComments(body, comments);
  }

  public FunctionDeclarationWithComments withComments(hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments) {
    return new FunctionDeclarationWithComments(body, comments);
  }
}
